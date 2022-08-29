library("khroma")
library("ggridges")
#library("lemon")

rainbow <- colour("smooth rainbow")
pal30 <- rainbow(30, range = c(0.05, 1))

cite_m <- assay(cite)
cite_prop <- prop.table(cite_m, margin = 2)

# Set xmin to 1 to be able to see differences in figure, otherwise zeroes
# dominate
cumulative_pct <- function(mat, ab = NULL, xmin = 1, xmax = 50){
    if (is.null(ab)) ab <- colnames(mat)
    prop_long <- prop.table(mat[, ab], margin = 2)
    row_means <- names(sort(rowMeans(prop_long)))
    prop_long <- prop_long %>%
        tibble::as_tibble(rownames = "ADT") %>%
        tidyr::pivot_longer(cols = -ADT, names_to = NULL) %>%
        dplyr::mutate(value = value * 100,
                      ADT = factor(ADT, levels = rev(row_means)))
    
    p <- ggplot(prop_long, aes(x = value, y = 1 - ..y..)) +
        stat_ecdf() +
        facet_wrap(~ADT) + 
        #lemon::facet_rep_wrap(~ADT, repeat.tick.labels = "bottom") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 90),
              axis.text.y = element_text(size = 6),
              strip.text.x = element_text(size = 6)) +
        coord_cartesian(xlim = c(xmax, xmin)) +
        labs(y = "Percentage of cells",
             x = "Minimum % of reads per cell", 
             title = "1 - Cumulative distribution of read % per cell by antigen")
    
    return(p)
}


# density ridges
xmax <- 5
p <- ggplot(prop_long, aes(x = value, y = ADT)) +
  ggridges::geom_density_ridges() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 90),
        axis.text.y = element_text(size = 6),
        strip.text.x = element_text(size = 6)) +
  coord_cartesian(xlim = c(xmin, xmax)) +
  labs(y = "Density",
       x = "% of reads within a cell")






ab_ords <- apply(cite_prop, 2, function(x){
    order(x[x > 0.05], decreasing = TRUE)
})






#min_adt_per_cell <- 
#read_cutoff <- 10
#cite_m[cite_m < read_cutoff] <- 0

cite_prop <- prop.table(cite_m, margin = 2)


ranks <- apply(-cite_prop, 2, rank)
frac_in_cell <- 0.05
ranks[cite_prop < frac_in_cell] <- 0

# percentage accounted for by top ranked antibody?

rank_to_idx





ab_ord <- order(rowMeans(cite_prop), decreasing = TRUE)


#max_val <- apply(cite_prop, 2, max)

# Which is the max and how much does it take up?
#orders <- apply(cite_prop, 2, function(x) order(x, decreasing = TRUE))
ranks <- apply(-cite_prop, 2, rank, ties.method = "random")
frac_in_cell <- 0.05
ranks[cite_prop < frac_in_cell] <- 0

group_by_top <- function(df, n){
    cn <- as.character(seq_len(n))
    dplyr::group_by(df, )
}

rank_long <- as_tibble(ranks, rownames = "ADT") %>%
    tidyr::pivot_longer(cols = -ADT) %>%
    dplyr::filter(! value == 0) %>%
    tidyr::pivot_wider(names_from = value,
                       values_from = ADT,
                       names_prefix = "r")

rank_long %>%
    group_by(!!!syms(sprintf("r%s", 1:4))) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) 


# top phenotypes by percentage reads, colour by average percentage of reads
# when it has that rank

# try rounding to nearest percentage and then arranging for group order
# round(test* 100, 0)

# or distribution of percentages by marker






# Group by the top n, combo, subsample from each group
# arrange by rowsum?

rr <- ranks
rr[rr > 4] = 0
rank_combs <- unique(rr, MARGIN = 2)


## cluster the prop table
dd <- dist(t(cite_prop))


n <- 100
ss <- sample(seq_len(ncol(cite_m)), n)

# in Witkowski, up to 12 Abs per cell meet rank cutoff


# cutoff on number of cells with prop meeting cutoff?



max_abs <- rownames(cite_prop)[apply(cite_prop, 2)]

# Order the rows, how many orderings are there?
ab_ords <- apply(cite_prop, 2, function(x){
    order(x[x > 0.1], decreasing = TRUE)
    })

dd <- dist(t(test_prop))
hc <- hclust(dd)

# Order by 
# when it is present, what is it's average
keep_ab <- names(sort(rowSums(cite_prop > 0.05), decreasing = TRUE)[1:20])
col_ord <- order(test_prop[keep_ab[1],], decreasing = TRUE)

# Transform and plot -----

prop_long <- as_tibble(test_prop[keep_ab, col_ord], rownames = "ADT") %>%
    tidyr::pivot_longer(cols = -ADT) %>%
    dplyr::filter(value >= 0.05) %>%
    dplyr::mutate(Percentage = value * 100,
                  ADT = factor(ADT, levels = rev(keep_ab)),
                  name = factor(name, levels = hc$labels[hc$order]))
                  
                  
                  
               #   across(where(is.character), as.factor))
    


ggplot(prop_long, aes(x = name, y = Percentage, fill = ADT)) +
    geom_bar(position = "stack",
             stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    labs(x = "Cell") + 
    scale_fill_manual(values = rev(pal30)) + 
    scale_y_continuous(expand = c(0, 0))


# Group by number of markers expressed



# Cutoff?

# Do you need a certain umi count before you see an antibody
# (because others are soaking up read space)


# Typical cell gating schema?


