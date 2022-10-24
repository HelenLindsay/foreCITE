wrtContext <- function(mat, marker, marker_pct = 0.2, pct_cutoff = 0.05){
    prop_t <-  prop.table(mat, margin = 2)
    # Marker of interest should be positive
    prop_t <- prop_t[, prop_t[marker, ] >= marker_pct]
    # Order columns by decreasing marker pct
    prop_t <- prop_t[, order(prop_t[marker, ], decreasing = TRUE)] %>%
        tibble::as_tibble(rownames = "ADT")
}



# Idea - noise reads should be equally distributed between contexts

plotNoiseDistn <- function(mat, pct_threshold = 20, n_per_cluster = 10){
    # Filter rows and columns of matrix first?s
    
    # note redundant, get contexts also calculates prop t
    pos_contexts <- foreCITE:::getContexts(mat, pct_threshold = pct_threshold)
    prop_t <- get_prop(mat)
    # Remove antibodies with all zeroes
    prop_t <- prop_t[rowSums(prop_t[, 2:ncol(prop_t)]) > 0, ]
    
    cell_to_cxt <- pos_contexts %>%
        dplyr::select(name, context) %>%
        unique()
    cn_to_cxt <- match(cell_to_cxt$name, colnames(prop_t))
    colnames(prop_t)[cn_to_cxt] <- cell_to_cxt$context
    
    # Collect all ungrouped cells into a "None" category
    # As it's a tibble, the first column is the marker name
    colnames(prop_t)[setdiff(seq_len(ncol(prop_t)), c(1, cn_to_cxt))] <- "None"

    prop_long <- tidyr::pivot_longer(prop_t, cols = -ADT) %>%
        dplyr::group_by(ADT, name) %>%
        dplyr::mutate(context_mean = mean(value))
    
    # Select top n per cluster, get mean (idea: are there cells which are close)
    prop_long_cut <- prop_long %>%
        dplyr::slice_max(value, n = n_per_cluster) %>%
        dplyr::summarise(sliced_mean = mean(value))
    
    
    # hacky way to get clustering order rather than making a proper heatmap
    
    # split by difference in mean in vs out of context
    
    prop_cut <- prop_long_cut %>%
        tidyr::pivot_wider(names_from = name,
                           values_from = sliced_mean) %>%
        ungroup()
    prop_cut_m <- as.matrix(prop_cut %>% dplyr::select(-ADT))
    rownames(prop_cut_m) <- prop_cut$ADT
    
    row_ord <- rownames(prop_cut_m)[hclust(dist(prop_cut_m))$order]
    col_ord <- colnames(prop_cut_m)[hclust(dist(t(prop_cut_m)))$order]
    
    prop_long_cut <- prop_long_cut %>%
        dplyr::mutate(ADT = factor(ADT, levels = row_ord),
                      name = factor(name, levels = col_ord)) %>%
        dplyr::mutate(in_context = grepl(ADT, name))
    
    
    spectral <- colorRampPalette(
        rev(RColorBrewer::brewer.pal(11, "Spectral")))
    
    p <- ggplot(prop_long_cut, aes(x = name, y = ADT, fill = sliced_mean)) +
        geom_tile() +
        theme_bw() +
        scale_fill_gradientn(colors = spectral(100),
                             limits = c(0, 0.2),
                             oob = scales::squish) +
        scale_x_discrete(position = "top") +
        theme(axis.text.x = element_text(angle = 90, size = 4,
                                         hjust = 0, vjust = 0.5),
              axis.text.y = element_text(size = 6)) + 
        labs(y = "Context")
    
   return(p)    
}

# Idea for reassigning:
# - does the child context exist?
# if yes, is a parent cell close (in the "positive" markers only) 


# If a context has a low positive peak, background should be high but
# balanced across markers (for witkowski - CD8A)

# CD137_TNFRSF9 - high value in "None" means that n cell threshold not reached 

# Expand contexts: distance measure to another context
# estimate mean and one-sided standard deviation?  

# Do all parent contexts exist?

# Mean of the upper quartile? or top n?  
# Just taking the mean not ideal for small subgroups

# To do: keep note of context membership
# Aggregation - if a cell is 

# Markers only positive in none - ncell cutoff too high?
# None category influencing clustering
# would be good to see if the markers have "positive peaks" - and if so does
# the pctage expression come from positive or negative peak
