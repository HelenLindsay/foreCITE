# rna_umi = named vector from colsums
pctByUmi <- function(mat, rna_umi){
    if( ! length(rna_umi) == ncol(mat)) {
        stop("Expecting mat to be ADT (rows) x cells (columns), ",
             "and rna_umi to be matching umi counts per cell for RNA")
    }
    
    prop_long <- get_prop_long(mat)
    
    
    # Bin manually?
    
    
}

# plot what happens to other positive markers
# filter where the marker of interest is definitely positive, 
# and the other marker is positive at least once
# pt = prop_table
# pt <- pt[, pt[marker,] >= 0.4 & mat[marker, ] >= 5]
#rm <- rowMaxs(pt)
# pt <- pt[rm >= 0.05, ]

# Don't want to do this as lose correspondence between cells
#pt_long <- tibble::as_tibble(pt, rownames = "ADT") %>% 
#    tidyr::pivot_longer(cols = -ADT) %>%
#    dplyr::filter(value >= 0.01)

#breaks <- seq(0,1, by = 0.05)
#pt_c <- cut(pt_c, breaks)

pt_marker <- pt_long %>% dplyr::filter(ADT == marker)
pt_marker <- dplyr::rename(pt_marker, marker_value = value) %>%
    dplyr::select(-ADT)
pt_long <- pt_long %>% left_join(pt_marker)
pt_long <- pt_long %>% dplyr::mutate(marker_value = 
                                         cut(marker_value, breaks = breaks))
