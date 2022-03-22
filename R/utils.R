# select_protein ----
#'@title select_protein
#'@description given a list of MultiAssayExperiments, select a given protein
#'and combine data into long format for plotting with ggplot2
#'@param mas (list(n)) a list of MultiAssayExperiments
#'@param protein (character(1)) the name of the protein to select
#'@param assay (character(1)) the name of the assay in the 
#'MultiAssayExperiments, default "ADT"
select_protein <- function(mas, protein, assay = "ADT"){
    res <- lapply(function(x) x[protein, , assay]) # Rows, Samples, Assays
    # Does this still return a multiassay experiment if only one assay is 
    # selected?
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


