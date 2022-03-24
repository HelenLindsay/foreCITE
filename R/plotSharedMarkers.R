# Would be good to include / exclude based on regex
#'@title plotSharedMarkers
#'@param sce A named list of SingleCellExperiments
#'@param markers A vector of markers to include (Default: NULL)
#'@param exclude A vector of markers to exclude (Default: NULL)
#'@param regex logical(1) Should include and exclude be treated as regular 
#'expressions?
plotSharedMarkers <- function(sce, markers = NULL, exclude = NULL, 
                              regex = TRUE){
    rn <- lapply(sce, rownames)
    df <- as.data.frame(table(data.frame(marker = unlist(rn),
                                         dataset = rep(names(sce), lengths(rn)),
                                         row.names = NULL)))
    
    
    # TO DO: ADD INCLUDE / EXCLUDE FILTERING
    if (! is.null(exclude)){
        if (length(exclude) > 1){
          exclude <- paste(exclude, collapse = "|")
        }

    }
    
    fill_colours <- c("0" = "white", "1" = "black", "2" = "red")
    
    ggplot(df, aes(y = marker, x = dataset, fill = as.character(Freq))) +
        geom_tile() +
        theme_bw() + 
        scale_fill_manual(values = fill_colours) + 
        theme(axis.text.y = element_text(size = 2),
              axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
        labs(x = NULL, y = NULL, fill = "Count")
}