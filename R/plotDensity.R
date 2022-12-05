#'@importFrom methods setGeneric
#'@export
setGeneric("adtDensity", signature = c("obj"),
           function(obj, ...) {
               standardGeneric("adtDensity")
           })


# adtDensity for signature matrix ------
#'@importFrom methods setMethod 
#'@export
setMethod("adtDensity", "Matrix")
function(obj, ..., assay = "ADT") {
    
}


# adtDensity for signature SingleCellExperiment ------
#'@importFrom methods setMethod 
#'@importFrom SingleCellExperiment assays 
#'@importClassesFrom SingleCellExperiment SingleCellExperiment
#'@export
setMethod("adtDensity", "SingleCellExperiment")
          function(obj, ..., assay = "ADT") {
              
              if (assay %in% names(assays(obj))){
                  return(adtDensity(assays(obj)[[assay]], ...))
              }
              is_alt <- assay %in% SingleCellExperiment::altExpNames(obj)
              
              if (isTRUE(is_alt)){
                  adt <- altExps(SingleCellExperiment)[[assay]]
                  return(adtDensity(adt, ...))
              } 
              
              stop(sprintf("Assay %s not found\n", assay))
}



# plotDensity ----
# Optional: for SingleCellExperiment signature, allow row filtering
#'@title plotDensity
#'@description Simple density plot of marker density
#'@param m A matrix of counts to plot, where rows are features
#'@param xlab character(1) x-axis label
#'@param group logical(1) Should each row be plotted separately?  If TRUE,
#'each row will be plotted in a separate colour (Default: FALSE)
#'@return a ggplot2 plot
plotDensity <- function(m, xlab = "Log10(count)", group = FALSE){
    m_long <- cells_to_long(m)
    if (isTRUE(group)){
        p <- ggplot2::ggplot(m_long, aes(x = value, color = tag)) +
            ggplot2::labs(colour = NULL)
    } else {
        p <- ggplot2::ggplot(m_long, aes(x = value))
    }
    p <- p +
        geom_density() +
        scale_x_log10() +
        labs(x = xlab) +
        theme_bw()
    return(p)
}


# Add option for mirrored density plot from package DataVisualizations

# Add flowCytometry style plot

# For titration datasets, dilution on x, intensity of staining on y


