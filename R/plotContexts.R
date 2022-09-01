# get_prop_long ----
# get prop.table, convert to long format
get_prop_long <- function(mat){
    mat <- mat[, colSums(mat) > 0]
    
    # Get read proportions
    prop_long <- prop.table(mat, margin = 2) %>%
        tibble::as_tibble(rownames = "ADT") %>%
        tidyr::pivot_longer(cols = -ADT)
    
    return(prop_long)
}


# getContexts -----
#'
#'@title Get antibody combinations in which a marker occurs
#'
#'@description Intended for exploratory analysis, contexts are not necessarily
#'genuine cell types.
#'
#'@param mat A matrix of raw antibody tag counts
#'@param pct_threshold  Minimum read percentage within a cell to consider 
#'(Default: 5)
#'@param min_cells Number of cells in which a marker should reach the minimum
#'percentage
getContexts <- function(mat, pct_threshold = 5, min_cells = 5){

    prop_long <- get_prop_long(mat)
    
    # Get contexts in which marker ever passes threshold
    
    # All cells appear in all_contexts table because above ensures one marker
    # must be non-zero 
    contexts <- prop_long %>%
        dplyr::filter(value >= pct_threshold/100.0) %>%
        dplyr::group_by(name) %>%
        
        # TO DO: speedup by not pasting until necessary? 
        dplyr::mutate(context = 
                          paste(sort(ADT, na.last = NA), collapse = "+"),
                      n_markers = length(! is.na(ADT)),
                      context = paste0(context, "+"))  %>%
        
        # At least min_cells must have all markers present together
        dplyr::group_by(context, name) %>%
        # Select cells there all markers in context meet cutoff 
        dplyr::filter(dplyr::n_distinct(ADT) == n_markers) %>%
        dplyr::group_by(context) %>%
        # At least (min_cells) must remain per context
        dplyr::filter(dplyr::n_distinct(name) >= min_cells) %>%
        dplyr::mutate(n_cells = dplyr::n_distinct(name)) %>%
        dplyr::ungroup()
        
    return(contexts)
}


# expandContexts ----
#
#'Expand contexts to include cells where one marker does not meet the threshold
#'@description Given a context identified selecting cells with a minimum
# expression in a minimum number of cells, expand context to include cells
# where one marker doesn't meet the threshold.  Cells may be in multiple
# expanded contexts, depending on marker of reference.
#'@param mat A matrix of raw ADT counts
#'@param adt (character(1)) The antibody-derived tag of interest
#'@param contexts A data.frame of contexts, produced by getContexts
expandContexts <- function(mat, adt, contexts){
    # For each context, get the parent context
    # (where marker of interest doesn't meet threshold)
    # Note that another marker is likely to cross the threshold,
    # which will not be reflected in parent context
    
    # TO DO: doing this with string formatting is a bit silly....
    contexts <- contexts %>%
        dplyr::mutate(parent_context =
                          stringr::str_replace(context, ADT, ""),
                      # Fix formatting
                      parent_context = 
                          gsub("^\\+|\\+(?=\\+)", "",
                               parent_context, perl = TRUE),
                      parent_context = dplyr::na_if(parent_context, ""))
    
     
    # Add the parent_contexts to each context 
    temp <- contexts %>%
        dplyr::filter(context %in% parent_context) %>%
        dplyr::select(-parent_context) %>%
        dplyr::rename(parent_context = context)

    contexts <- full_join(contexts, temp) %>%
        dplyr::filter(! is.na(context))
    
    return(contexts)
        
}


# contextDensity -----
# 
# Plot density of read percentages by marker context
#'@param contexts A context matrix, created by getContexts
#'@param adt character(1) The name of the antibody tag of interest
#'@return a ggplot2 plot
#'@importFrom ggplot scale_x_continuous
contextDensity <- function(contexts, mat, adt){
    # TO DO: add reference distribution at the top
    # Add marker presence tile at the side
    # Add tile of how many cells are involved
    
    contexts <- contexts %>%
        dplyr::filter(ADT == adt) %>%
        dplyr::group_by(context) %>%
        dplyr::mutate(quantile = quantile(value, 0.75)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(quantile)) %>%
        dplyr::mutate(context = factor(context, levels = unique(context)))
    
    p <- ggplot(contexts, aes(x = value, y = context)) +
        ggridges::geom_density_ridges(scale = 3, alpha = 0.75) +
        scale_x_continuous(expand = c(0.01,0)) + 
        theme_bw() +
        theme(axis.text.y = element_text(size = 4)) +
        labs(x = "Proportion of reads", y = "Context",
             title = sprintf("Distribution of read proportions for %s", marker))
    
    prop_long <- get_prop_long(mat) 
    
    
    return(p)
}