# cumulative_pct ----
# TO DO: 
# - allow xmin and xmax to be unspecified
# - directly input a single cell experiment / multi-assay experiment.

#' Plot cumulative antibody percentage across cells
#'
#'@description Plots the cumulative percentage of total antibody-derived tags
#'(ADT) accounted for by each antibody.  Equal to 1 - empirical cumulative
#'density function of antibody percentage per cell.  The x-axis shows the
#'percentage of ADT reads, and the y-axis shows the fraction of cells where
#'this antibody is present at the indicated percentage or higher. 
#'
#'This plot is intended to highlight antibodies that have high background
#'binding or consume a large fraction of reads because they bind a ubuquitous
#'target protein.  Empty droplets should be filtered out before running this
#'function.  As this is intended to be a quick exploratory plot, cells are
#'downsampled by default.
#'
#'@param mat
#'@param ab
#'@param xmin (integer(1)).  Minimum value for x axis.  Default is 1 so that
#'figures are not dominated by zeroes.
#'@param xmax
#'@param n (Integer(1)) How many antibodies should be shown?  If specified,
#'will select the most abundant antibodies by mean percentage across cells.
#'(Default: NULL means show all)
#'@param subsample Number of cells to subsample if more are available, or NULL
#'if cells should never be subsampled (Default: 100000) 
#'@importFrom ggplot2 stat_ecdf
#'@importFrom ggplot2 facet_wrap
#'@importFrom ggplot2 theme_bw
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 coord_cartesian
#'@importFrom ggplot2 labs
cumulative_pct <- function(mat, ab = NULL, xmin = 1, xmax = 50, n = NULL){
    # Remove columns with zero antibody counts
    mat <- mat[, colSums(mat) > 0]
  
    if ("dgCMatrix" %in% class(mat)){
        stop("This function doesn't work for class dgCMatrix")
    }
    
    if (is.null(ab)) ab <- rownames(mat)
    
    prop_long <- prop.table(mat[ab, ], margin = 2)
    row_means <- names(sort(BiocGenerics::rowMeans(prop_long)))
    
    ## TO DO: IF THERE ARE TOO MANY TO SEE, PAGINATE?
    
    if (! is.null(n)){
        row_means <- row_means[1:min(nrow(mat), n)]
        prop_long <- prop_long[row_means, ]
    }
    
    prop_long <- prop_long %>%
      tibble::as_tibble(rownames = "ADT") %>%
      tidyr::pivot_longer(cols = -ADT, names_to = NULL) %>%
      dplyr::mutate(value = value * 100,
                    ADT = factor(ADT, levels = rev(row_means)))
    
    p <- ggplot(prop_long, aes(x = value, y = 1 - ..y..)) +
      stat_ecdf() +
      facet_wrap(~ADT) + 
      theme_bw() +
      theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
            axis.text.y = element_text(size = 6),
            strip.text.x = element_text(size = 6)) +
      coord_cartesian(xlim = c(xmax, xmin)) +
      labs(y = "Fraction of cells",
           x = "Minimum % of reads per cell", 
           title = "1 - (Cumulative distribution of read % per cell by antigen)")
    
    return(p)
}

