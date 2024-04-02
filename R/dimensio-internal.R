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
#  It will only be mapped if at least one [graphical parameters][graphics::par]
#  is explicitly specified (see examples).
#' @param col The colors for lines and points.
#' @param bg The background color for the open plot symbols given by `pch = 21:25`.
#' @param pch A vector of plotting characters or symbols. This can either be
#' a single character or an integer code for one of a set of graphics symbols.
#' @param cex A numerical vector giving the amount by which plotting characters
#'  and symbols should be scaled relative to the default.
#' @param lty,lwd A specification for the line type and width.
#' @param ... Currently not used.
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
                    col = NULL, bg = NULL, pch = 16, cex = NULL,
                    lty = NULL, lwd = NULL, alpha = FALSE) {
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
  if (reorder) {
    if (length(col) == n) col <- col[origin]
    if (length(bg) == n) bg <- bg[origin]
    if (length(pch) == n) pch <- pch[origin]
    if (length(lty) == n) lty <- lty[origin]
    if (length(cex) == n) cex <- cex[origin]
    if (length(lwd) == n) lwd <- lwd[origin]
  }

  ## Recode
  data$observation <- "active"
  data$observation[data$supplementary] <- "suppl."

  ## Highlight
  if (length(highlight) == 1) {
    high <- NULL
    if (has_extra(x)) {
      high <- get_extra(x)[[highlight]]
    }
    if (is.null(high)) {
      choices <- c("mass", "sum", "contribution", "cos2", "observation")
      highlight <- match.arg(highlight, choices = choices, several.ok = FALSE)
      high <- data[[highlight]]
    }
    highlight <- high
  }

  ## Graphical parameters
  ## Colors
  col <- scale_color(x = highlight, col = col, alpha = alpha)
  bg <- scale_color(x = highlight, col = bg, alpha = alpha)

  if (length(pch) == 1) pch <- rep(pch, length.out = n)
  if (length(lty) == 1) lty <- rep(lty, length.out = n)
  if (length(cex) == 1) cex <- rep(cex, length.out = n)
  if (length(lwd) == 1) lwd <- rep(lwd, length.out = n)
  if (!is.double(highlight)) { # Discrete scales
    ## Symbol
    pch <- scale_symbol(x = highlight, symb = pch, what = "pch")
    ## Line type
    lty <- scale_symbol(x = highlight, symb = lty, what = "lty")
    ## Size
    cex <- cex %||% graphics::par("cex")
    ## Line width
    lwd <- lwd %||% graphics::par("lwd")
  } else { # Continuous scales
    ## Symbol
    pch <- pch %||% graphics::par("pch")
    ## Line type
    lty <- lty %||% graphics::par("lty")
    ## Size
    cex <- scale_size(x = highlight, size = cex, what = "cex")
    ## Line width
    lwd <- scale_size(x = highlight, size = lwd, what = "lwd")
  }

  coord <- data.frame(
    x = data[[1]],
    y = data[[2]],
    z = highlight %||% character(n),
    label = data$label,
    sup = data$supplementary,
    col = col,
    bg = bg,
    pch = pch,
    cex = cex,
    lty = lty,
    lwd = lwd
  )

  ## Subset
  if (active & !sup) coord <- coord[!coord$sup, , drop = FALSE]
  if (!active & sup) coord <- coord[coord$sup, , drop = FALSE]

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
      im <- grDevices::as.raster(x$col)

      pr <- pretty(h, n = ifelse(nrow(x) > 5, 5, nrow(x)))
      pr <- pr[pr <= max(h) & pr >= min(h)]
      i <- order(h, method = "radix")[!duplicated(h)]

      col <- grDevices::colorRamp(x$col[i])(arkhe::scale_range(pr, from = range(h)))
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

scale_color <- function(x, col = NULL, alpha = FALSE) {
  if (is.null(x) || all(is.na(x))) {
    col <- col %||% graphics::par("col")
    return(col)
  }
  if (length(col) == length(x)) return(col)

  if (is.double(x)) {
    ## Continuous scale
    col <- arkhe::palette_color_continuous(x, palette = col)
    if (alpha) col <- grDevices::adjustcolor(col, alpha.f = alpha)
  } else {
    ## Discrete scale
    col <- arkhe::palette_color_discrete(x, palette = col)
  }

  col
}
scale_symbol <- function(x, symb = NULL, what = "pch") {
  if (is.null(x) || all(is.na(x))) {
    symb <- symb %||% graphics::par(what)
    return(symb)
  }
  if (length(symb) == length(x)) return(symb)

  arkhe::palette_shape(x = x, palette = symb)
}
scale_size <- function(x, size = NULL, what = "cex") {
  if (is.null(x) || all(is.na(x))) {
    size <- size %||% graphics::par(what)
    return(size)
  }
  if (length(size) == length(x)) return(size)

  arkhe::palette_size(x = x, palette = size)
}

