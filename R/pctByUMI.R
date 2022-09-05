# rna_umi = named vector from colsums
pctByUmi <- function(mat, rna_umi){
    if( ! length(rna_umi) == ncol(mat)) {
        stop("Expecting mat to be ADT (rows) x cells (columns), ",
             "and rna_umi to be matching umi counts per cell for RNA")
    }
    
    prop_long <- get_prop_long(mat)
    
    
    # Bin manually?
    
    
}