# PLOT ELLIPSE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_wrap
#' @aliases viz_tolerance,MultivariateAnalysis-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 1, axes = c(1, 2), group = NULL,
                        level = 0.95, color = NULL, fill = FALSE, symbol = FALSE) {
    .viz_ellipse(x, ..., type = "tolerance", level = level,
                 margin = margin, axes = axes, group = group,
                 color = color, fill = fill, symbol = symbol)
  }
)

#' @export
#' @rdname viz_wrap
#' @aliases viz_tolerance,BootstrapCA-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "BootstrapCA"),
  definition = function(x, ..., margin = 1, axes = c(1, 2), level = 0.95,
                        color = FALSE, fill = FALSE, symbol = FALSE) {
    group <- get_groups(x, margin = margin)
    methods::callNextMethod(x, margin = margin, axes = axes,
                            group = group, level = level,
                            color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_wrap
#' @aliases viz_confidence,MultivariateAnalysis-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 1, axes = c(1, 2), group = NULL,
                        level = 0.95, color = NULL, fill = FALSE, symbol = FALSE) {
    .viz_ellipse(x, ..., type = "confidence", level = level,
                 margin = margin, axes = axes, group = group,
                 color = color, fill = fill, symbol = symbol)
  }
)

#' @export
#' @rdname viz_wrap
#' @aliases viz_confidence,BootstrapCA-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "BootstrapCA"),
  definition = function(x, ..., margin = 1, axes = c(1, 2), level = 0.95,
                        color = FALSE, fill = FALSE, symbol = FALSE) {
    group <- get_groups(x, margin = margin)
    methods::callNextMethod(x, margin = margin, axes = axes,
                            group = group, level = level,
                            color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

.viz_ellipse <- function(x, ..., type = c("tolerance", "confidence"),
                         level = 0.95, margin = 1, axes = c(1, 2),
                         group = NULL, color = NULL, fill = FALSE, symbol = FALSE) {
  fun <- switch(
    type,
    tolerance = wrap_tolerance,
    confidence = wrap_confidence
  )
  ell <- fun(x, margin = margin, axes = axes, group = group, level = level)
  n <- length(ell)

  ## Recycle graphical parameters if of length one
  dots <- list(...)
  col <- recycle(dots$border %||% graphics::par("fg"), n)
  bg <- recycle(dots$col %||% NA, n)
  lty <- recycle(dots$lty %||% graphics::par("lty"), n)
  lwd <- recycle(dots$lwd %||% graphics::par("lwd"), n)

  if (n > 1) {
    ## Discrete scales
    extra_quali <- names(ell)
    if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(extra_quali)
    if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(extra_quali)
    if (!isFALSE(symbol)) lty <- khroma::palette_line(types = symbol)(extra_quali)
  }

  for (i in seq_along(ell)) {
    lvl <- ell[[i]]
    for (j in seq_along(lvl)) {
      graphics::polygon(x = lvl[[j]], border = col[i],
                        col = bg[i], lty = lty[i], lwd = lwd[i])
    }
  }

  invisible(x)
}
