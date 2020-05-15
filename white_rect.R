#' Add white rectangles to a ggplot
#' 
#' @param xmin,xmax,ymin,ymax Coordinates of the rectangle's edges
#' @param color Filling and border color of the rectangle. Defaults to white.
#' 
#' @return A `geom` to add to a `ggplot`

# Function to stick a layer of white rectangles to hide some parts
white_rect <- function(xmin, xmax, ymin, ymax, color = "white") {
  ggplot2::geom_rect(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = color, 
    fill = color
  )
}
