# GGPLOT2
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

StatHull <- ggplot2::ggproto(
  `_class` = "StatHull",
  `_inherit` = ggplot2::Stat,
  compute_group = function(data, scales) {
    i <- chull(data$x, data$y)
    data[c(i, i[1]), , drop = FALSE]
  },
  required_aes = c("x", "y")
)

#' @rdname envelopes
#' @export
stat_hull <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatHull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
