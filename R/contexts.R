# getContexts -----
#'
#'@title Get antibody combinations in which a marker occurs
#'
#'@description Intended for exploratory analysis, contexts are not necessarily
#'genuine cell types.
#'
#'@param prop_t A table of proportions, in long format (use get_prop_long to
#'create it)
#'@param pct_threshold  Minimum read percentage within a cell to consider 
#'(Default: 5)
#'@param min_cells Number of cells in which a marker should reach the minimum
#'percentage
#'@export
getContexts <- function(prop_long, pct_threshold = 5, min_cells = 5){
  
  # Get contexts in which marker ever passes threshold
  
  # All cells appear in all_contexts table because above ensures one marker
  # must be non-zero 
  contexts <- prop_long %>%
    dplyr::filter(value >= pct_threshold/100.0) %>%
    dplyr::group_by(name) %>% # Here name = cell
    
    # TO DO: speedup by not pasting until necessary? list(unique(ADT))
    # Note that sorting here is alphabetic
    
    # Order by - how many contexts it appears in?
    dplyr::mutate(context = 
                    paste(sort(ADT, na.last = NA), collapse = "+"),
                  n_markers = length(! is.na(ADT)),
                  context = paste0(context, "+"))  %>%
    
    # At least min_cells must have all markers present together
    dplyr::group_by(context, name) %>%
    # Select cells where all markers in context meet cutoff 
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
