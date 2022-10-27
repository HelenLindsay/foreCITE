# Did the antibody work?
# - could be a good antibody with rare cell population
# -> look at cooccurrence with other ADT
# Does the antibody look like in other studies 


pctByCount <- function(mat){
    mat_long <- as_tibble(mat, rownames = "ADT") %>%
        tidyr::pivot_longer(cols = -ADT, values_to = "count") 

    prop_cont <- left_join(prop_long, mat_long)
    x <- prop_cont %>% filter(ADT == "CD18")

    p <- ggplot(x, aes(x = count, y = value)) +
        geom_bin2d() +
        theme_bw() +
        scale_fill_continuous("Cell count", type = "viridis", trans = "log10") +
        labs(y = "Proportion of ADT reads", x = "Count")

    # Get current x axis limits 
    x_scales <- layer_scales(p)$x$get_limits()

    dx <- ggplot(x, aes(x = count)) +
        geom_density() +
        theme_minimal() +
        #scale_y_reverse() +
        xlim(x_scales) +
        labs(x = NULL, y = "Density")

    patchwork::wrap_plots(dx, p, nrow = 2)
}