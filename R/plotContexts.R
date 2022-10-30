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