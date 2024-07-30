# HELPERS

#' Weighted Column Means and Standard Deviations
#'
#' @param x A [`numeric`] matrix.
#' @param w An [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
weighted_mean <- function(x, w) {
  as.vector(crossprod(w, x))
}
weighted_sd <- function(x, w) {
  sqrt(as.vector(crossprod(w, x^2)))
}

#' Column Index
#'
#' @param index A [`numeric`] vector.
#' @param n An [`integer`] value.
#' @param names A [`character`] vector.
#' @return A [`logical`] vector.
#' @keywords internal
#' @noRd
find_variable <- function(index, n, names = NULL) {
  x <- logical(n)

  if (is.null(index)) return(x)

  if (is.logical(index)) {
    arkhe::assert_length(index, n)
    return(index)
  }

  if (is.character(index)) {
    index <- match(index, names)
    index <- index[!is.na(index)]
    if (length(index) == 0) return(x)
  }

  if (is.numeric(index)) {
    x[index] <- TRUE
    return(x)
  }

  arkhe::assert_type(index, "numeric")
}

#' Remove Columns Using a Predicate
#'
#' @param x A [`data.frame`].
#' @param what A predicate [`function`].
#' @param negate A [`logical`] scalar: should the negation of `f` be used
#'  instead of `f`?
#' @param sup A `vector` specifying the indices of the supplementary columns.
#' @param extra A `vector` specifying the indices of the extra columns.
#' @param what A [`character`] string to be used in the message.
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @details
#'  Side effect: move `sup` and `extra` columns at the end of `x`.
#' @return A `list` with the following elements: `data` (a `data.frame`),
#'  `sup` (an `integer` vector) and `extra` (a `data.frame` or `NULL`).
#' @keywords internal
#' @noRd
drop_variable <- function(x, f, negate = FALSE, sup = NULL, extra = NULL,
                          what = "extra", verbose = getOption("dimensio.verbose")) {
  if (negate) f <- Negate(f)
  not_ok <- vapply(x, FUN = f, FUN.VALUE = logical(1))

  if (any(not_ok)) {
    is_extra <- find_variable(extra, ncol(x), names = colnames(x))
    is_sup <- find_variable(sup, ncol(x), names = colnames(x))

    old <- x
    x <- x[, !(not_ok | is_sup | is_extra), drop = FALSE]

    if (any(is_sup)) {
      ## Move supplementary variables at the end
      sup <- seq_len(sum(is_sup)) + ncol(x)
      x <- cbind(x, old[, is_sup, drop = FALSE])
    }
    if (any(is_extra)) {
      ## Remove extra variable
      extra <- old[, is_extra, drop = FALSE]
    }

    # Generate message
    not_ok[is_sup | is_extra] <- FALSE
    if (any(not_ok) && verbose) {
      tot <- sum(not_ok)
      msg <- "%d %s %s removed: %s."
      txt <- ngettext(tot, "variable was", "variables were")
      col <- paste(colnames(old)[not_ok], collapse = ", ")
      message(sprintf(msg, tot, what, txt, col))
    }
  }

  list(
    data = x,
    sup = sup,
    extra = extra
  )
}

#' Prepare Data for Plotting
#'
#' @param x A [`MultivariateAnalysis-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript
#'  which the data will be returned: `1` indicates individuals/rows (the
#'  default), `2` indicates variables/columns.
#' @param axes A length-two [`numeric`] vector giving the dimensions to be
#'  plotted.
#' @param principal A [`logical`] scalar: should principal coordinates be
#'  returned? If `FALSE`, standard coordinates are returned.
#' @param active A [`logical`] scalar: should the active observations be
#'  plotted?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  plotted?
#' @param highlight A vector specifying the information to be highlighted.
#'  If `NULL` (the default), no highlighting is applied. If a single `character`
#'  string is passed, it must be the name of a categorical variable, or one of
#'  "`observation`", "`mass`", "`sum`", "`contribution`" or "`cos2`"
#'  (see [`augment()`]).
#' @param color The colors for lines and points (will be mapped to `highlight`).
#'  Ignored if set to `FALSE`.
#' @param shape A vector of plotting characters or symbols (will be mapped to
#'  `highlight`). This can either be a single character or an integer code for
#'  one of a set of graphics symbols. Ignored if set to `FALSE`.
#' @param size A length-two [`numeric`] vector giving range of possible sizes
#'  (greater than 0; will be mapped to `highlight`). Ignored if set to `FALSE`.
#' @param line_type,line_width A specification for the line type and width (will
#'  be mapped to `highlight`). Ignored if set to `FALSE`.
#' @return
#'  A [`data.frame`] with the following columns:
#'    \describe{
#'     \item{`x`}{Coordinates along x.}
#'     \item{`y`}{Coordinates along y.}
#'     \item{`z`}{Variable to be highlighted.}
#'     \item{`label`}{Label.}
#'     \item{`sup`}{Is supplementary?}
#'     \item{`col`}{Color for lines and points.}
#'     \item{`bg`}{Background color.}
#'     \item{`pch`}{Symbol.}
#'     \item{`cex`}{Symbol size.}
#'     \item{`lty`}{Line type.}
#'     \item{`lwd`}{Line width.}
#'    }
#' @author N. Frerebeau
#' @keywords internal
prepare <- function(x, margin, axes = c(1, 2), active = TRUE,
                    sup = TRUE, principal = TRUE,
                    highlight = NULL, reorder = TRUE,
                    color = NULL, fill = FALSE,
                    shape = NULL, size = c(1, 6),
                    line_type = NULL, line_width = size, ...) {
  ## Prepare data
  data <- augment(x, margin = margin, axes = axes, principal = principal)
  n <- nrow(data)

  ## Reorder
  ## /!\ see build_results() /!\
  origin <- get_order(x, margin = margin)
  if (length(highlight) > 1) {
    arkhe::assert_length(highlight, n)
    if (reorder) highlight <- highlight[origin]
  }

  ## Recode
  data$observation <- ifelse(data$supplementary, "suppl.", "active")

  ## Highlight
  if (length(highlight) == 1) {
    high <- NULL
    ## Look for a variable in the original data
    if (has_extra(x)) {
      high <- get_extra(x)[[highlight]]
    }
    ## If nothing is found, look for statistical information
    if (is.null(high)) {
      choices <- c("mass", "sum", "contribution", "cos2", "observation")
      highlight <- match.arg(highlight, choices = choices, several.ok = FALSE)
      high <- data[[highlight]]
    }
    highlight <- high
  }

  ## Graphical parameters
  f <- function(x, n) {
    if (length(x) == 1) x <- rep(x, n)
    x
  }
  dots <- list(...)
  col <- f(dots$col %||% graphics::par("col"), n)
  bg <- f(dots$bg %||% graphics::par("bg"), n)
  pch <- f(dots$pch %||% 16, n)
  cex <- f(dots$cex %||% graphics::par("cex"), n)
  lty <- f(dots$lty %||% graphics::par("lty"), n)
  lwd <- f(dots$lwd %||% graphics::par("lwd"), n)

  if (!is.null(highlight)) {
    if (!is.double(highlight)) {
      ## Discrete scales
      if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(highlight)
      if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(highlight)
      if (!isFALSE(shape)) pch <- khroma::palette_shape(symbols = shape)(highlight)
      if (!isFALSE(line_type)) lty <- khroma::palette_line(types = line_type)(highlight)
    } else {
      ## Continuous scales
      if (!isFALSE(color)) col <- khroma::palette_color_continuous(colors = color)(highlight)
      if (!isFALSE(fill)) bg <- khroma::palette_color_continuous(colors = fill)(highlight)
      if (!isFALSE(size)) cex <- khroma::palette_size_range(range = size)(highlight)
      if (!isFALSE(line_width)) lwd <- khroma::palette_size_range(range = line_width)(highlight)
    }
  } else {
    highlight <- character(n)
  }

  coord <- data.frame(
    data,
    x = data[[1L]],
    y = data[[2L]],
    z = highlight,
    label = data$label,
    col = col,
    bg = bg,
    pch = pch,
    cex = cex,
    lty = lty,
    lwd = lwd
  )

  ## Subset
  if (active & !sup) coord <- coord[!coord$supplementary, , drop = FALSE]
  if (!active & sup) coord <- coord[coord$supplementary, , drop = FALSE]

  coord
}

#' Build a Legend
#'
#' @param x A [`data.frame`] returned by [prepare()].
#' @param args A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param points A [`logical`] scalar: legend for points?
#' @param lines A [`logical`] scalar: legend for lines?
#' @author N. Frerebeau
#' @keywords internal
prepare_legend <- function(x, args, points = TRUE, lines = TRUE) {
  h <- x$z
  h <- h[!is.na(h)]
  if (!is.null(h) && length(unique(h)) > 1 && is.list(args) && length(args) > 0) {
    if (is.double(h)) {
      ## Continuous scale
      # im <- grDevices::as.raster(x$col)

      pr <- pretty(h, n = ifelse(nrow(x) > 5, 5, nrow(x)))
      pr <- pr[pr <= max(h) & pr >= min(h)]
      i <- order(h, method = "radix")[!duplicated(h)]

      col <- grDevices::colorRamp(x$col[i])(scale_range(pr, from = range(h)))
      col <- grDevices::rgb(col, maxColorValue = 255)

      leg <- list(legend = pr, col = col)
      if (points) {
        cex <- stats::approx(x = h[i], y = x$cex[i], xout = pr, ties = "ordered")$y
        leg <- utils::modifyList(leg, list(pch = unique(x$pch), pt.cex = cex))
      }
      if (lines) {
        lwd <- stats::approx(x = h[i], y = x$lwd[i], xout = pr, ties = "ordered")$y
        leg <- utils::modifyList(leg, list(lty = unique(x$lty), lwd = lwd))
      }
    } else {
      ## Discrete scale
      param <- stats::aggregate(
        x[, c("col", "bg", "pch", "lty")],
        by = list(z = x$z),
        FUN = unique
      )
      leg <- list(legend = param$z, col = param$col)
      if (points) {
        leg <- utils::modifyList(leg, list(pt.bg = param$bg, pch = param$pch))
      }
      if (lines) {
        leg <- utils::modifyList(leg, list(lty = param$lty))
      }
    }

    leg <- utils::modifyList(leg, args)
    do.call(graphics::legend, args = leg)
  }
}
