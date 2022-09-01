#library("lemon") # put x axis on every facet

# density ridges -----
# doesn't really work, most are too stacked at the 0
density_ridge_plot <- function(prop_long){
    xmax <- 5
    p <- ggplot(prop_long, aes(x = value, y = ADT)) +
      ggridges::geom_density_ridges(scale = 5) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 6, angle = 90),
            axis.text.y = element_text(size = 6),
            strip.text.x = element_text(size = 6)) +
      coord_cartesian(xlim = c(xmin, xmax)) +
      labs(y = "Density",
           x = "% of reads within a cell")
}

# distribution of counts coloured by percentage -----
# or count versus percentage in cell


# order top antibodies (dplyr method better) -----
#ab_ords <- apply(cite_prop, 2, function(x){
#    order(x[x > 0.05], decreasing = TRUE)
#})

# get ranks above percentage cutoff -----
#ranks <- apply(-cite_prop, 2, rank)


# Plot proportion of reads in different contexts -----
frac_in_cell <- 0.05
min_cells <- 50

## problem - this ignores all the times the marker occurs in the context 
## at less than the proportion
#prop_long <- as_tibble(cite_prop, rownames = "ADT") %>%
#    tidyr::pivot_longer(cols = -ADT) %>%
#    dplyr::filter(value >= frac_in_cell) %>%
#    dplyr::group_by(name) %>%
#    dplyr::mutate(context = paste(sort(ADT, na.last = NA), collapse = "+")) %>%

# this will never be more than number of markers in context because rows are 
# filtered for minimum marker proportion
#    dplyr::group_by(name, context) %>%
#    dplyr::mutate(n_cells = n(),
#                  mean_prop = mean(value)) %>%
#    dplyr::filter(n >= min_cells)


# Want the proportion of times the marker reaches the threshold within a given
# context

n_cells <- ncol(cite_prop)

# These are contexts where the marker ever reaches proportion threshold
prop_long <- as_tibble(cite_prop, rownames = "ADT") %>%
    tidyr::pivot_longer(cols = -ADT)

# All cells appear in all_contexts table because by def'n one marker must be
# non-zero 
all_contexts <- prop_long %>%
    dplyr::filter(value >= frac_in_cell) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(context = paste(sort(ADT, na.last = NA), collapse = "+")) %>%
    dplyr::group_by(context) %>%
    dplyr::mutate(n_cells = n_distinct(name))
    


    # Contexts with and without a high background marker

    # In how many contexts does this marker reach the proportion cutoff?
    
    # We now want to look at the proportion of times the marker reaches
    # threshold in this context.
    
    # (if all the other markers are expressed
    # how often do we get marker of interest proportion at least as high
    # verusus in other contexts)    



# Select a marker and plot - most have too many contexts
marker <- "IGM"
test <- prop_long %>%
    dplyr::filter(ADT == "CD64") %>%
    dplyr::arrange(mean_prop)# %>%
    #dplyr::mutate(context = factor(context, levels = unique(context))) 
    
ggplot(test, aes(x = value, y = context)) +
    ggridges::geom_density_ridges(scale = 2) +
    theme_bw() +
    theme(axis.text.y = element_text(size = 4)) +
    labs(x = "Proportion of reads", y = "Context",
         title = sprintf("Distribution of read proportions for %s", marker))


# Table of top ranking antibody combinations -----







ranks <- apply(-cite_prop, 2, rank, ties.method = "random")
frac_in_cell <- 0.05
ranks[cite_prop < frac_in_cell] <- 0

n_contexts <- 3
n_cn <- sprintf("r%s", seq_len(n_contexts))
    
group_by_top <- function(df, n){
    group_by(df, !!!syms(sprintf("r%s", seq_len(n))))
}


rank_long <- as_tibble(ranks, rownames = "ADT") %>%
    tidyr::pivot_longer(cols = -ADT) %>%
    dplyr::filter(! value == 0) %>%
    tidyr::pivot_wider(names_from = value,
                       values_from = ADT,
                       names_prefix = "r") 


#top_contexts <- rank_long %>%
#    group_by(!!!syms(sprintf("r%s", seq_len(n_contexts)))) %>%
#    summarise(n = n()) %>%
#    arrange(desc(n)) %>%
#    tidyr::unite("context_id", !!!syms(n_cn), sep = "+", na.rm = TRUE)

#rank_long <- rank_long %>%
#    # Add an ID column
#    #tidyr::unite("context_id", !!!syms(n_cn),
#    #             remove = FALSE, sep = "+", na.rm = TRUE) %>%
#
#   rowwise() %>%
#   dplyr::mutate(context_id = 
#                      paste(do.call(sort, !!!syms(n_cn)), sep = "+"))
#    
#    # select just the name and context id
#    dplyr::select(name, context_id)
 


   

min_reads <- 5

marker_by_context <- function(cite_m, marker, min_reads, min_cells = 50, 
                              n_contexts = 20){
    rr <- t(cite_m[marker, cite_m[marker,] >= min_reads, drop = FALSE]) 
    rr <- as_tibble(rr, rownames = "name") %>%
        dplyr::left_join(rank_long) 
    
    top_contexts <- rr %>%
        dplyr::group_by(context_id) %>%
        dplyr::summarise(n = n(), mean_exp = mean(!!sym(marker))) %>%
        dplyr::filter(n >= min_cells) %>%
        dplyr::arrange(desc(n)) %>%
        head(n_contexts) %>%
        dplyr::pull(context_id)
    
    rr <- rr %>%
        dplyr::filter(context_id %in% top_contexts)
    
    p <- ggplot(rr, aes(x = !!ensym(marker), y = context_id)) +
        ggridges::geom_density_ridges(scale = 2) +
        coord_cartesian() +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 90),
              axis.text.y = element_text(size = 6),
              strip.text.x = element_text(size = 6)) +
        labs(y = "Density",
             x = "Read count")
    
}






# Subsample cells, cluster ------

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

# Barplot top percentages -----

prop_long <- as_tibble(test_prop[keep_ab, col_ord], rownames = "ADT") %>%
    tidyr::pivot_longer(cols = -ADT) %>%
    dplyr::filter(value >= 0.05) %>%
    dplyr::mutate(Percentage = value * 100,
                  ADT = factor(ADT, levels = rev(keep_ab)),
                  name = factor(name, levels = hc$labels[hc$order]))
                  
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


# Notes -----

# Cutoff?

# Do you need a certain umi count before you see an antibody
# (because others are soaking up read space)

# Are "background" reads evenly distributed?

# Typical cell gating schema?


# top phenotypes by percentage reads, colour by average percentage of reads
# when it has that rank

# try rounding to nearest percentage and then arranging for group order
# round(test* 100, 0)

# Group by the top n, combo, subsample from each group
# arrange by rowsum?

# Amongst cells that have the same combination of markers, do we see 