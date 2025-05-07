# PLOT ELLIPSE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_ellipses
#' @aliases viz_ellipses,numeric,numeric-method
setMethod(
  f = "viz_ellipses",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ..., group = NULL,
                        type = c("tolerance", "confidence"), level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    type <- match.arg(type, several.ok = FALSE)
    fun <- switch(
      type,
      tolerance = wrap_tolerance,
      confidence = wrap_confidence
    )
    ell <- fun(x, y, group = group, level = level)
    .viz_ellipses(ell, color = color, fill = fill, symbol = symbol, ...)

    invisible(list(x = x, y = y))
  }
)

#' @export
#' @rdname viz_ellipses
#' @aliases viz_ellipses,MultivariateAnalysis,missing-method
setMethod(
  f = "viz_ellipses",
  signature = c(x = "MultivariateAnalysis", y = "missing"),
  definition = function(x, ..., group = NULL,
                        type = c("tolerance", "confidence"), level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    type <- match.arg(type, several.ok = FALSE)
    fun <- switch(
      type,
      tolerance = wrap_tolerance,
      confidence = wrap_confidence
    )
    ell <- fun(x, margin = get_margin(), axes = get_axes(),
               group = group, level = level, principal = get_principal())
    .viz_ellipses(ell, color = color, fill = fill, symbol = symbol, ...)

    invisible(x)
  }
)

#' @export
#' @rdname viz_ellipses
#' @aliases viz_ellipses,PCOA,missing-method
setMethod(
  f = "viz_ellipses",
  signature = c(x = "PCOA", y = "missing"),
  definition = function(x, ..., group = NULL,
                        type = c("tolerance", "confidence"), level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    type <- match.arg(type, several.ok = FALSE)
    fun <- switch(
      type,
      tolerance = wrap_tolerance,
      confidence = wrap_confidence
    )
    ell <- fun(x, axes = get_axes(), group = group, level = level)
    .viz_ellipses(ell, color = color, fill = fill, symbol = symbol, ...)

    invisible(x)
  }
)


#' @param x A `list` of `matrix` returned by [wrap_ellipse()].
#' @noRd
.viz_ellipses <- function(x, ..., color = NULL, fill = FALSE, symbol = FALSE) {
  n <- length(x)

  ## Recycle graphical parameters if of length one
  dots <- list(...)
  col <- recycle(dots$border %||% graphics::par("fg"), n)
  bg <- recycle(dots$col %||% NA, n)
  lty <- recycle(dots$lty %||% graphics::par("lty"), n)
  lwd <- recycle(dots$lwd %||% graphics::par("lwd"), n)

  if (n > 1) {
    ## Discrete scales
    extra_quali <- names(x)
    if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(extra_quali)
    if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(extra_quali)
    if (!isFALSE(symbol)) lty <- khroma::palette_line(types = symbol)(extra_quali)
  }

  for (i in seq_along(x)) {
    lvl <- x[[i]]
    for (j in seq_along(lvl)) {
      graphics::polygon(
        x = lvl[[j]],
        border = col[i],
        col = bg[i],
        lty = lty[i],
        lwd = lwd[i]
      )
    }
  }

  invisible(x)
}

# Tolerance ====================================================================
#' @export
#' @rdname viz_tolerance
#' @aliases viz_tolerance,numeric,numeric-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ..., group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, y, group = group, type = "tolerance", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(list(x = x, y = y))
  }
)

#' @export
#' @rdname viz_tolerance
#' @aliases viz_tolerance,MultivariateAnalysis,missing-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "MultivariateAnalysis", y = "missing"),
  definition = function(x, ..., group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = group, type = "tolerance", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_tolerance
#' @aliases viz_tolerance,MultivariateBootstrap,missing-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "MultivariateBootstrap", y = "missing"),
  definition = function(x, ..., level = 0.95,
                        color = FALSE, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = NULL, type = "tolerance", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_tolerance
#' @aliases viz_tolerance,PCOA,missing-method
setMethod(
  f = "viz_tolerance",
  signature = c(x = "PCOA", y = "missing"),
  definition = function(x, ..., group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = group, type = "tolerance", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

# Confidence ===================================================================
#' @export
#' @rdname viz_confidence
#' @aliases viz_confidence,numeric,numeric-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ..., group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, y, group = group, type = "confidence", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(list(x = x, y = y))
  }
)

#' @export
#' @rdname viz_confidence
#' @aliases viz_confidence,MultivariateAnalysis,missing-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "MultivariateAnalysis", y = "missing"),
  definition = function(x, ..., group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = group, type = "confidence", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_confidence
#' @aliases viz_confidence,MultivariateBootstrap,missing-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "MultivariateBootstrap", y = "missing"),
  definition = function(x, ..., level = 0.95,
                        color = FALSE, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = NULL, type = "confidence", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_confidence
#' @aliases viz_confidence,PCOA,missing-method
setMethod(
  f = "viz_confidence",
  signature = c(x = "PCOA", y = "missing"),
  definition = function(x, ..., axes = c(1, 2), group = NULL, level = 0.95,
                        color = NULL, fill = FALSE, symbol = FALSE) {
    viz_ellipses(x, group = group, type = "confidence", level = level,
                 color = color, fill = fill, symbol = symbol, ...)
    invisible(x)
  }
)
