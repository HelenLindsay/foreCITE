wrtContext <- function(mat, marker, marker_pct = 0.2, pct_cutoff = 0.05){
    prop_t <-  prop.table(mat, margin = 2)
    # Marker of interest should be positive
    prop_t <- prop_t[, prop_t[marker, ] >= marker_pct]
    # Order columns by decreasing marker pct
    prop_t <- prop_t[, order(prop_t[marker, ], decreasing = TRUE)] %>%
        tibble::as_tibble(rownames = "ADT")
    
}