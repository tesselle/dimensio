# PLOT CONVEX HULL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_wrap
#' @aliases viz_hull,MultivariateAnalysis-method
setMethod(
  f = "viz_hull",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL, ...) {
    hull <- wrap_hull(x, margin = margin, axes = axes, group = group)
    n <- length(hull)

    ## Graphical parameters
    border <- list(...)$border %||% graphics::par("col")
    col <- list(...)$col %||% NA
    lty <- list(...)$lty %||% graphics::par("lty")
    lwd <- list(...)$lwd %||% graphics::par("lwd")
    if (length(border) != n) border <- rep(border, length.out = n)
    if (length(col) != n) col <- rep(col, length.out = n)
    if (length(lty) != n) lty <- rep(lty, length.out = n)
    if (length(lwd) != n) lwd <- rep(lwd, length.out = n)

    for (i in seq_along(hull)) {
      graphics::polygon(x = hull[[i]], border = border[i],
                        col = col[i], lty = lty[i], lwd = lwd[i])
    }

    invisible(x)
  }
)

#' @export
#' @rdname viz_wrap
#' @aliases viz_hull,BootstrapCA-method
setMethod(
  f = "viz_hull",
  signature = c(x = "BootstrapCA"),
  definition = function(x, margin = 1, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = margin)
    methods::callNextMethod(x, margin = margin, axes = axes, group = group, ...)
    invisible(x)
  }
)
