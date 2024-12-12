#' Draw Court
#'
#' This is a function that draws the tennis court (dimensions are to scale)
#'
#' @return returns a ggplot object with solid lines representing the lines on the tennis court, dashed line represents the net
#'
#' @examples
#' draw_court()
#'
#' @import tidyverse
#' @export

draw_court <- function() {
  court <- ggplot() +
    annotate(geom = "segment", y = 5.02, yend = 5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
    annotate(geom = "segment", y = 4.11, yend = 4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
    annotate(geom = "segment", y = -5.02, yend = -5.02, x = -11.88, xend = 11.88, alpha = 0.5) +
    annotate(geom = "segment", y = -4.11, yend = -4.11, x = -11.88, xend = 11.88, alpha = 0.5) +
    annotate(geom = "segment", y = 0, yend = 0, x = -6.4, xend = 6.4, alpha = 0.5) +
    annotate(geom = "segment", x = 0, xend = 0, y = -5.02, yend = 5.02,
             linetype = 2, alpha = 0.5) +
    annotate(geom = "segment", x = -11.88, xend = -11.88, y = -5.02,
             yend = 5.02, alpha = 0.5) +
    annotate(geom = "segment", x = 11.88, xend = 11.88, y = -5.02,
             yend = 5.02, alpha = 0.5) +
    annotate(geom = "segment", x = -6.4, xend = -6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
    annotate(geom = "segment", x = 6.4, xend = 6.4, y = -4.11, yend = 4.11, alpha = 0.5) +
    annotate(geom = "segment", y = 0, yend = 0, x = -11.88, xend = -11.6, alpha = 0.5) +
    annotate(geom = "segment", y = 0, yend = 0, x = 11.88, xend = 11.6, alpha = 0.5) +
    theme_void() +
    coord_fixed()

  return(court)
}
