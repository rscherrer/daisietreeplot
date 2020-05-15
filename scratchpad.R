rm(list = ls())

library(tidyverse)
library(ggtree)

source("white_rect.R")
source("single_branch.R")
source("daisietreeplot.R")

set.seed(42)

# Random trees
t1 <- ape::rtree(10)
t1$tip.label <- gsub("t", "t1.", t1$tip.label)
t2 <- ape::rtree(3)
t2$tip.label <- gsub("t", "t2.", t2$tip.label)
t3 <- single_branch("t3.1", edge.length = 4.6) # tree with one species
trees <- list(t1, t2, t3)
names(trees) <- c("A", "B", "C")

# Toy colonization events for each clade
tcols <- c(4.5, 5, 4.6)

# Toy metadata
metadata <- tibble::tibble(
  clade = names(trees), 
  endemic = TRUE,  # whether each clade is endemic
  uncertain = FALSE  # whether colonization time is known for sure
)
metadata$endemic[3] <- FALSE
metadata$uncertain[2] <- TRUE

# Island age
age <- 5

# Make a plot
p <- daisietreeplot(
  trees, 
  age, 
  tcols, 
  metadata, 
  mapping = ggplot2::aes(color = endemic, linetype = uncertain, shape = uncertain),
  pargs = list(size = 3)
)

# Customize it
p <- p + 
  ggplot2::scale_color_manual(
    values = c("forestgreen", "darkblue"), na.translate = FALSE
  ) +
  ggplot2::ggtitle("Island colonization by multiple clades") +
  ggplot2::xlab("Time (Mya)") +
  ggplot2::guides(lty = FALSE) +
  ggplot2::scale_shape(na.translate = FALSE)
p

ggsave("figure.png", p, height = 5, width = 4, dpi = 300)
