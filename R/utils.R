# select_protein ----
#'@title select_protein_mas
#'@description given a list of MultiAssayExperiments, select a given protein
#'and combine data into long format for plotting with ggplot2
#'@param mas (list(n)) a list of MultiAssayExperiments
#'@param protein (character(1)) the name of the protein to select
#'@param assay (character(1)) the name of the assay in the 
#'MultiAssayExperiments, default "ADT"
select_protein_mas <- function(mas, protein, assay = "ADT"){
    res <- lapply(function(x) x[protein, , assay]) # Rows, Samples, Assays
    # Returns a MultiAssayExperiment
}


#'@title select_protein_sce
#'@description select a row from each of a named list of SingleCellExperiments
#'@param sce list(n) a list of SingleCellExperiments
#'@export
select_protein_sce <- function(sce, protein){
    if (is.null(names(sce))) {
        stop("List of SingleCellExperiments must be named")
    }
    keep <- vapply(sce, function(x) protein %in% rownames(x), logical(1))
    if (! any(keep)){
      stop(sprintf("Protein %s not found in rownames of any dataset"))
    }
      
    sce <- sce[keep]
    counts <- lapply(sce, function(x) unname(assay(x)[protein, ]))
    nms <- rep(names(sce), lengths(counts))
    df <- data.frame(dataset = nms, count = unlist(counts))
    rownames(df) <- NULL

    return(df)    
    # keep as sce if colData needed
}




# cells_to_long ----
#'@title cells_to_long
#'@description Take a matrix, convert to data.frame, add rownames as
#'a column named tag.name and convert to long format for plotting
#'@param m a matrix, usually of droplet counts
#'@param tag.name character(1) the name of the column derived from row.names
cells_to_long <- function(m, tag.name = "tag"){
    cells_long <- m %>%
        as.data.frame() %>%
        tibble::rownames_to_column(tag.name) %>%
        tidyr::pivot_longer(-all_of(tag.name))
}


# print_paginated ----
#'@title print_paginated
#'@description function for printing a ggplot2 facet plot with many variables
#'across several pages using ggforce::facet_wrap_paginate
#'@param p A ggplot2 plot, prior to applying facet_wrap
#'@param facet character(1) Name of column to use for faceting
#'@param npages numeric(1) Number of pages to split the plot into (Default: 6)
#'@param nrow numeric(1) Number of rows of plots per page (Default: 3)
#'@param ncol numeric(1) (Default: 3)
#'@param ...    Additional arguments for facet_wrap
print_paginated <- function(p, facet, npages = 6, nrow = 3, ncol = 3, ...){
    for (i in c(1:npages)){
        print(i)
        print(p + ggforce::facet_wrap_paginate(as.formula(paste("~", facet)),
                                      ncol = ncol, nrow = nrow, page = i, ...))
    }
}


