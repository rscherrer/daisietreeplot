#' Single-branch phylogenetic tree
#' 
#' @param tip.label Name of the unique tip
#' @param edge.length Length of the unique edge
#' 
#' @return A phylogenetic tree in `phylo` format.
#' 
#' @examples 
#' 
#' # A tree with one species with a 15Myr-old stem
#' single_branch("some species", edge.length = 15)
#' 
#' @export

# Function to display a single branch
single_branch <- function(tip.label = "t1", edge.length = 1) {
  tree <- list(
    edge = matrix(c(2, 1), 1, 2),
    tip.label = tip.label,
    edge.length = edge.length,
    Nnode = 1
  )
  class(tree) <- "phylo"
  tree
}
