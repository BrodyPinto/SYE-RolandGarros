#' Draw Court
#'
#' This is a function that draws the tennis court (dimensions are to scale)
#'
#' @return returns ggplot layers drawing solid lines representing the lines on the tennis court, dashed line represents the net
#'
#' @examples
#' draw_court()
#'
#' @import tidyverse
#' @export

## TODO: flip x and y
draw_court <- function() {
  list(
    annotate(geom = "segment", x = 5.02, xend = 5.02, y = -11.88, yend = 11.88, alpha = 0.5),
    annotate(geom = "segment", x = 4.11, xend = 4.11, y = -11.88, yend = 11.88, alpha = 0.5),
    annotate(geom = "segment", x = -5.02, xend = -5.02, y = -11.88, yend = 11.88, alpha = 0.5),
    annotate(geom = "segment", x = -4.11, xend = -4.11, y = -11.88, yend = 11.88, alpha = 0.5),
    annotate(geom = "segment", x = 0, xend = 0, y = -6.4, yend = 6.4, alpha = 0.5),
    annotate(geom = "segment", y = 0, yend = 0, x = -5.02, xend = 5.02, linetype = 2, alpha = 0.5),
    annotate(geom = "segment", y = -11.88, yend = -11.88, x = -5.02, xend = 5.02, alpha = 0.5),
    annotate(geom = "segment", y = 11.88, yend = 11.88, x = -5.02, xend = 5.02, alpha = 0.5),
    annotate(geom = "segment", y = -6.4, yend = -6.4, x = -4.11, xend = 4.11, alpha = 0.5),
    annotate(geom = "segment", y = 6.4, yend = 6.4, x = -4.11, xend = 4.11, alpha = 0.5),
    annotate(geom = "segment", x = 0, xend = 0, y = -11.88, yend = -11.6, alpha = 0.5),
    annotate(geom = "segment", x = 0, xend = 0, y = 11.88, yend = 11.6, alpha = 0.5),
    theme_void(),
    coord_fixed()
  )
}
