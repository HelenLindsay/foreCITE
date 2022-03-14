# plotDensity ----
# Optional: for SingleCellExperiment signature, allow row filtering
#'@title plotDensity
#'@description Simple density plot of marker density
#'@param m A matrix of counts to plot, where rows are features
#'@param xlab character(1) x-axis label
#'@param group logical(1) Should each row be plotted separately?  If TRUE,
#'each row will be plotted in a separate colour (Default: FALSE)
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
}



