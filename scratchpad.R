rm(list = ls())

library(ggtree)
library(tidyverse)
library(ape)
library(phytools)
library(tidytree)

# Function to display a single branch
single_branch <- function(tip.label, edge.length = 1) {
  tree <- list(
    edge = matrix(c(2, 1), 1, 2),
    tip.label = tip.label,
    edge.length = edge.length,
    Nnode = 1
  )
  class(tree) <- "phylo"
  tree
}

# Function to stick a layer of white rectangles to hide some parts
white_rect <- function(xmin, xmax, ymin, ymax) {
  geom_rect(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = "white", 
    fill = "white"
  )
}

# Random trees
t1 <- rtree(10)
t1$tip.label <- gsub("t", "t1.", t1$tip.label)
t2 <- rtree(3)
t2$tip.label <- gsub("t", "t2.", t2$tip.label)
t3 <- single_branch("t3.1")
trees <- list(t1, t2, t3)

# Toy colonization events for each clade
tcols <- c(3, 4, 1)
names(tcols) <- c("A", "B", "C")

# Toy tip dataset
spnames <- lapply(trees, "[[", "tip.label") %>% do.call("c", .)
spnames <- spnames[spnames != "new"]
clades <- c(rep("A", 10), rep("B", 3), "C")
tips <- tibble(label = spnames, clade = clades)
tips$endemic <- TRUE
tips$endemic[tips$clade == "C"] <- FALSE

tips

# Toy clade dataset
clades <- tibble(clade = levels(tips$clade), endemic = TRUE)
clades$endemic[3] <- FALSE
clades$tcols <- c(4, 4, 4)

clades

# Island age
age <- 5

# Keep this small
xlen <- 0.1

# Takes a list of trees
# No need to be ultrametric
# The trees should be in phylo format
# For single-branch trees, use single_branch()
# The trees do not need a stem or a root-edge, will be overwritten
# Make sure that the age of each tree is smaller than the island age

daisietreeplot <- function(trees, tcols, age, clades, mapping = NULL, xlen = 0.001) {
  
  # For each tree...
  trees <- lapply(trees, function(tree) {
    
    # Add an extra branch around the age of the island (this is a hack)
    crown <- max(node.depth.edgelength(tree))
    pos <- age - crown
    tree$root.edge <- pos
    bind.tip(tree, "new", edge.length = xlen, where = "root", position = pos)
    
  })
  
  # Concatenate all the trees together
  tree <- reduce(trees, bind.tree)
  
  # Total number of tips
  ntips <- Ntip(tree)
  
  # Plot the meta-tree
  p <- ggtree(tree, ladderize = FALSE)
  p <- revts(p)
  p <- p + theme_tree2()
  
  # Hide the extra tips near the island age (those are here to make sure that 
  # the stems of the different subtrees go all the way to the island age)
  p <- p + ylim(c(1, ntips))
  p <- p + white_rect(-age, -age + xlen, 1, ntips)
  
  # For each clade, hide the stem up to the colonization event 
  ymax <- 0
  ymin <- 0

  for (i in seq_along(trees)) {
    ymin <- ymax + 1
    ymax <- ymin + Ntip(trees[[i]]) - 1
    p <- p + white_rect(-age, -tcols[i], ymin, ymax)
  }
  
  # Add a dashed line for island emergence
  p <- p + geom_vline(xintercept = -age, lty = 2)
  
  # Append clade data to the plot
  p <- p %<+% clades
  
  # Identify the most recent common ancestor of each clade and its y-value
  mrca <- p$data %>% 
    filter(isTip & label %in% clades$label) %>%
    group_by(clade) %>%
    nest() %>%
    mutate(mrca = sapply(data, function(x) {
      out <- getMRCA(tree, x$label)
      if (is.null(out)) out <- x$node[1]
      out
    })) %>%
    mutate(y = p$data$y[p$data$node == mrca]) %>%
    select(clade, mrca, y)
  
  # Add colonization times of the MRCA
  if (is.null(names(tcols))) names(tcols) <- mrca$clade
  mrca <- mrca %>% right_join(tibble(clade = names(tcols), tcol = tcols), by = "clade")
  
  # Add clade-level metadata
  mrca <- mrca %>% right_join(
    clades %>% group_by(clade) %>% summarize_all(first) %>% select(-label),
    by = "clade"
  )
  
  # Identify internal nodes within each clade
  node_clades <- lapply(mrca$mrca, function(node) {
    out <- node
    if (node > Ntip(tree)) out <- c(out, offspring(tree, node))
    return(sort(out))
  })
  
  # Function to extract clade-level data for a given node
  get_clade <- function(node, variable, node_clades, mrca) {
    where <- sapply(node_clades, function(nodes) node %in% nodes)
    if (any(where)) mrca[[variable]][which(where)] else NA 
  }
  
  # Identify the clade-level variables to assign
  metacols <- colnames(clades)
  metacols <- metacols[metacols != "label"]
  
  # Label internal nodes with clade-level data
  p$data[, metacols] <- lapply(metacols, function(col) {
    sapply(p$data$node, get_clade, col, node_clades, mrca)
  })
  
  # Apply the mapping
  if (!is.null(mapping)) p <- p + mapping
  
  # Add points at colonization events
  p <- p + geom_point(data = mrca, aes(x = -tcol, y = y))
  
  p
  
}

p <- daisietreeplot(trees, tcols, age = 5, clades = clades, mapping = aes(color = endemic))

p <- p + 
  scale_color_manual(values = c("forestgreen", "darkblue"), na.translate = FALSE) +
  ggtitle("Island colonization by multiple clades") +
  xlab("Time (Mya)")
p

ggsave("figure.png", p, width = 4, height = 5, dpi = 300)
