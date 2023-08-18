# PLOT ELLIPSE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_wrap
#' @aliases viz_tolerance,MultivariateAnalysis-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL, level = 0.95, ...) {
    .viz_ellipse(x, type = "tolerance", level = level,
                 margin = margin, axes = axes, group = group, ...)
  }
)

#' @export
#' @rdname viz_wrap
#' @aliases viz_confidence,MultivariateAnalysis-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL, level = 0.95, ...) {
    .viz_ellipse(x, type = "confidence", level = level,
                 margin = margin, axes = axes, group = group, ...)
  }
)

.viz_ellipse <- function(x, type = c("tolerance", "confidence"), level = 0.95,
                         margin = 1, axes = c(1, 2), group = NULL, ...) {
  fun <- switch(
    type,
    tolerance = wrap_tolerance,
    confidence = wrap_confidence
  )
  ell <- fun(x, margin = margin, axes = axes, group = group, level = level)
  n <- length(ell)

  ## Graphical parameters
  border <- list(...)$border %||% graphics::par("col")
  col <- list(...)$col %||% NA
  lty <- list(...)$lty %||% graphics::par("lty")
  lwd <- list(...)$lwd %||% graphics::par("lwd")
  if (length(border) != n) border <- rep(border, length.out = n)
  if (length(col) != n) col <- rep(col, length.out = n)
  if (length(lty) != n) lty <- rep(lty, length.out = n)
  if (length(lwd) != n) lwd <- rep(lwd, length.out = n)

  for (i in seq_along(ell)) {
    lvl <- ell[[i]]
    for (j in seq_along(lvl)) {
      graphics::polygon(x = lvl[[j]], border = border[i],
                        col = col[i], lty = lty[i], lwd = lwd[i])
    }
  }

  invisible(x)
}
