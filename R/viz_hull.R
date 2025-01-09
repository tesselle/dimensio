# PLOT CONVEX HULL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_ellipses
#' @aliases viz_hull,MultivariateAnalysis-method
setMethod(
  f = "viz_hull",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 1, axes = c(1, 2), group = NULL,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    hull <- wrap_hull(x, margin = margin, axes = axes, group = group)
    n <- length(hull)

    ## Recycle graphical parameters if of length one
    dots <- list(...)
    col <- recycle(dots$border %||% graphics::par("fg"), n)
    bg <- recycle(dots$col %||% NA, n)
    lty <- recycle(dots$lty %||% graphics::par("lty"), n)
    lwd <- recycle(dots$lwd %||% graphics::par("lwd"), n)

    if (n > 1) {
      ## Discrete scales
      extra_quali <- names(hull)
      if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(extra_quali)
      if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(extra_quali)
      if (!isFALSE(symbol)) lty <- khroma::palette_line(types = symbol)(extra_quali)
    }

    for (i in seq_along(hull)) {
      graphics::polygon(x = hull[[i]], border = col[i],
                        col = bg[i], lty = lty[i], lwd = lwd[i])
    }

    invisible(x)
  }
)

#' @export
#' @rdname viz_ellipses
#' @aliases viz_hull,BootstrapCA-method
setMethod(
  f = "viz_hull",
  signature = c(x = "BootstrapCA"),
  definition = function(x, ..., margin = 1, axes = c(1, 2),
                        color = FALSE, fill = FALSE, symbol = FALSE) {
    group <- get_groups(x, margin = margin)
    methods::callNextMethod(x, margin = margin, axes = axes, group = group,
                            color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)
