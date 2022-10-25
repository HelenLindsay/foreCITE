# Subset mat to return cells where marker is positive, with cols ordered by
# marker percent
wrtContext <- function(mat, marker, marker_pct = 0.2){
    # Get ADT proportions per cell
    prop_t <-  proportions(mat, margin = 2)
    # Keep cells where marker of interest is positive (at least marker_pct)
    prop_t <- prop_t[, prop_t[marker, ] >= marker_pct]
    # Order columns by decreasing marker pct
    prop_t <- prop_t[, order(prop_t[marker, ], decreasing = TRUE)] %>%
        tibble::as_tibble(rownames = "ADT")
}



# Idea - noise reads should be equally distributed between contexts
# Note that if there are a lot of markers they may never reach threshold

#@pct_threshold Percentage (Default: 15)
#@n_per_context Number of cells ranked by read proportion to use
# in each point - note that they will not be the same cells across markers
# (Default: 10) 
#@tolerance (Default 5)
plotNoiseDistn <- function(mat, pct_threshold = 15, n_per_context = 10,
                           tolerance = 5){
    
    # Note that rows and columns are filtered in get_prop
    prop_long <- get_prop_long(mat, filter_pct = 5, filter_ncells = 5) 
    
    # TO DO: INTEGRATE THIS INTO getContexts
    # Get contexts -----
    # Get clear contexts - put a cell size cutoff too?
    # If a cell is within "tolerance" of being assigned to a clear (child)?
    # context, assign it to that context.
    pos_contexts <- getContexts(prop_long, pct_threshold = pct_threshold)
    exp_contexts <- getContexts(prop_long,
                                pct_threshold = pct_threshold - tolerance)
    # Only keep contexts that meet the original threshold
    exp_contexts <- exp_contexts %>%
        dplyr::select(-n_cells) %>%
        dplyr::filter(context %in% pos_contexts$context)
    
    pos_contexts <- pos_contexts %>%
        dplyr::select(-n_cells) %>%
        # keep only cells that aren't in exp_contexts
        dplyr::filter(! name %in% pos_contexts$name) %>%
        # replace original values with expanded contexts
        dplyr::full_join(exp_contexts)
    
    # Match cells to contexts ----
    cell_to_cxt <- pos_contexts %>%
        dplyr::select(name, context) %>%
        unique()
    
    # Cells without an assigned context will be labelled "None"
    cn_to_cxt <- structure(rep("None", ncol(mat)),
                           names = colnames(mat))
    cn_to_cxt[cell_to_cxt$name] <- cell_to_cxt$context
    
    # Get summary statistic by context and marker ----
    # Select top n per cluster, get mean
    # (idea: want to know if noise is evenly distributed)
    prop_long_cut <- prop_long %>%
        dplyr::mutate(name = cn_to_cxt[name]) %>%
        dplyr::group_by(ADT, name) %>%
        dplyr::slice_max(value, n = n_per_context) %>%
        dplyr::summarise(sliced_mean = mean(value))
    
    # Order for heatmap ----
    
    # hacky way to get clustering order rather than making a proper heatmap
    # split by difference in mean in vs out of context?
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
                      name = factor(name, levels = col_ord)) 
    
    # Plot ----
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

# On average, what pct do the "positive" markers account for?