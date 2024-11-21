# PLOT COORDINATES
#' @include AllGenerics.R
NULL

# Rows =========================================================================
#' @export
#' @rdname viz_individuals
#' @aliases viz_rows,MultivariateAnalysis-method
setMethod(
  f = "viz_rows",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, extra_quali = NULL, extra_quanti = NULL,
                        color = NULL, fill = FALSE, symbol = FALSE, size = c(1, 6),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 1, axes = axes, ...,
               active = active, sup = sup, labels = labels,
               extra_quali = extra_quali, extra_quanti = extra_quanti,
               color = color, fill = fill, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend)
    invisible(x)
  }
)

#' @export
#' @rdname viz_individuals
#' @aliases viz_rows,BootstrapCA-method
setMethod(
  f = "viz_rows",
  signature = c(x = "BootstrapCA"),
  definition = function(x, ..., axes = c(1, 2), color = FALSE, fill = FALSE,
                        symbol = FALSE) {
    viz_points(x, margin = 1, axes = axes, ..., active = TRUE, sup = TRUE,
               labels = FALSE, extra_quali = NULL,
               color = color, fill = fill, symbol = symbol)
    invisible(x)
  }
)

# Individuals ==================================================================
#' @export
#' @rdname viz_individuals
#' @aliases viz_individuals,PCA-method
setMethod(
  f = "viz_individuals",
  signature = c(x = "PCA"),
  definition = function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, extra_quali = NULL, extra_quanti = NULL,
                        color = NULL, fill = FALSE, symbol = FALSE, size = c(1, 6),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 1, axes = axes, ...,
               active = active, sup = sup, labels = labels,
               extra_quali = extra_quali, extra_quanti = extra_quanti,
               color = color, fill = fill, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend)
    invisible(x)
  }
)

# Columns =====================================================================
#' @export
#' @rdname viz_variables
#' @aliases viz_columns,MultivariateAnalysis-method
setMethod(
  f = "viz_columns",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, extra_quali = NULL, extra_quanti = NULL,
                        color = NULL, fill = FALSE, symbol = FALSE, size = c(1, 6),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 2, axes = axes, ...,
               active = active, sup = sup, labels = labels,
               extra_quali = extra_quali, extra_quanti = extra_quanti,
               color = color, fill = fill, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend)
    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_columns,MultivariateBootstrap-method
setMethod(
  f = "viz_columns",
  signature = c(x = "MultivariateBootstrap"),
  definition = function(x, ..., axes = c(1, 2), color = FALSE, fill = FALSE,
                        symbol = FALSE) {
    viz_points(x, ..., margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, extra_quali = NULL,
               color = color, fill = fill, symbol = symbol)
    invisible(x)
  }
)

# Variables ====================================================================
#' @export
#' @rdname viz_variables
#' @aliases viz_variables,PCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "PCA"),
  definition = function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = list(filter = "contribution", n = 10),
                        extra_quali = NULL, extra_quanti = NULL,
                        color = NULL, symbol = NULL, size = 1,
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    ## Prepare data
    coord <- prepare_plot(x, margin = 2, axes = axes, ...,
                          active = active, sup = sup,
                          extra_quali = extra_quali, extra_quanti = extra_quanti,
                          color = color, line_type = symbol, line_width = size)

    ## Save and restore graphical parameters
    ## pty: square plotting region, independent of device size
    old_par <- graphics::par(pty = "s", no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- xlim %||% range(coord$x, na.rm = TRUE, finite = TRUE)
    if (is_scaled(x)) xlim <- c(-1, 1)
    ylim <- ylim %||% range(coord$y, na.rm = TRUE, finite = TRUE)
    if (is_scaled(x)) ylim <- c(-1, 1)
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    graphics::abline(h = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
    graphics::abline(v = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))

    ## Scaled variables?
    if (is_scaled(x)) {
      circle(x = 0, y = 0, radius = 1, lwd = 1,
             border = graphics::par("fg"), n = 100)
    }

    graphics::arrows(
      x0 = 0, y0 = 0, x1 = coord$x, y1 = coord$y, length = 0.15, angle = 30,
      col = coord$col,
      lty = coord$lty,
      lwd = coord$lwd
    )

    ## Labels
    if (isTRUE(labels)) labels <- list()
    if (is.list(labels)) {
      viz_labels(coord, filter = labels$filter, n = labels$n)
    }

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct axis (axes)
    if (TRUE) {
      graphics::axis(side = 1, las = 1)
      graphics::axis(side = 2, las = 1)
    }

    ## Plot frame (frame.plot)
    if (TRUE) {
      graphics::box()
    }

    ## Add annotation (ann)
    if (TRUE) {
      graphics::title(
        main = main, sub = sub,
        xlab = print_variance(x, axes[[1]]),
        ylab = print_variance(x, axes[[2]])
      )
    }

    ## Legend
    prepare_legend(coord, legend, points = FALSE, lines = TRUE)

    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_variables,CA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "CA"),
  definition = function(x, ..., axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, extra_quali = NULL, extra_quanti = NULL,
                        color = NULL, fill = FALSE, symbol = FALSE, size = c(1, 6),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 2, axes = axes, ...,
               active = active, sup = sup, labels = labels,
               extra_quali = extra_quali, extra_quanti = extra_quanti,
               color = color, fill = fill, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_variables,BootstrapPCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "BootstrapPCA"),
  definition = function(x, ..., axes = c(1, 2), color = FALSE, fill = FALSE,
                        symbol = FALSE) {
    viz_points(x, ..., margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, extra_quali = NULL,
               color = color, fill = fill, symbol = symbol)
    invisible(x)
  }
)

# Helpers ======================================================================
#' Build a Factor Map
#'
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param labels A [`logical`] scalar: should labels be drawn? Labeling a large
#'  number of points can be computationally expensive and make the graph
#'  difficult to read. A selection of points to label can be provided using a
#'  `list` of two named elements, `filter` (a string specifying how to filter
#'  the labels to be drawn) and `n` (an integer specifying the number of labels
#'  to be drawn). See examples below.
#' @param xlim A length-two [`numeric`] vector giving the x limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param ylim A length-two [`numeric`] vector giving the y limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param ann A [`logical`] scalar: should the default annotation (title and x
#'  and y axis labels) appear on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Currently not used.
#' @inheritParams prepare_plot
#' @author N. Frerebeau
#' @keywords internal
viz_points <- function(x, margin, axes, ...,
                       active = TRUE, sup = TRUE,
                       labels = list(filter = "contribution", n = 10),
                       extra_quali = NULL, extra_quanti = NULL,
                       color = NULL, fill = FALSE,
                       symbol = NULL, size = c(1, 6),
                       xlim = NULL, ylim = NULL,
                       main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                       ann = graphics::par("ann"), frame.plot = TRUE,
                       panel.first = NULL, panel.last = NULL,
                       legend = list(x = "topleft")) {
  ## Prepare data
  coord <- prepare_plot(x, margin = margin, axes = axes,
                        active = active, sup = sup,
                        extra_quali = extra_quali,
                        extra_quanti = extra_quanti,
                        color = color, fill = fill,
                        symbol = symbol, size = size, ...)

  ## Save and restore graphical parameters
  ## pty: square plotting region, independent of device size
  old_par <- graphics::par(pty = "s", no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim %||% range(coord$x, na.rm = TRUE, finite = TRUE)
  ylim <- ylim %||% range(coord$y, na.rm = TRUE, finite = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::abline(h = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  graphics::abline(v = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  graphics::points(
    x = coord$x,
    y = coord$y,
    col = coord$col,
    bg = coord$bg,
    pch = coord$pch,
    cex = coord$cex
  )

  ## Labels
  if (isTRUE(labels)) labels <- list()
  if (is.list(labels)) {
    viz_labels(coord, filter = labels$filter, n = labels$n)
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct axis (axes)
  if (TRUE) {
    graphics::axis(side = 1, las = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    graphics::title(
      main = main, sub = sub,
      xlab = xlab %||% print_variance(x, axes[[1]]),
      ylab = ylab %||% print_variance(x, axes[[2]])
    )
  }

  ## Legend
  prepare_legend(coord, legend, points = TRUE, lines = FALSE)

  invisible(coord)
}

#' Non-Overlapping Text Labels
#'
#' @param x A [`data.frame`] (typically returned by [prepare_plot()]).
#' @param filter A [`character`] string specifying the variable used to filter
#'  observations. If `NULL`, all labels are drawn.
#' @param n An [`integer`] specifying the number of labels to draw.
#'  Only the labels of the top \eqn{n} observations according to `filter` will
#'  be drawn. If `NULL`, all labels are drawn.
#' @param type A [`character`] string specifying the shape of the field.
#'  It must be one of "`text`", "`shadow`" or "`box`". Any unambiguous substring
#'  can be given.
#' @param ... Currently not used.
#' @details
#'  Only labels in the plotting region (given by `par("usr")`) will be drawn.
#' @author N. Frerebeau
#' @keywords internal
viz_labels <- function(x, filter = "contribution", n = 10,
                       type = "shadow", ...) {
  ## Select
  if (!is.null(filter) && !is.null(n) && n > 0) {
    top <- min(nrow(x), n)
    how <- x[[filter]]
    k <- order(how, decreasing = TRUE)[seq_len(top)] # Get order
    x <- x[k, , drop = FALSE] # Subset
  }

  ## Filter
  xlim <- graphics::par("usr")[c(1, 2)]
  ylim <- graphics::par("usr")[c(3, 4)]
  x_filter <- x$x >= min(xlim) & x$x <= max(xlim)
  y_filter <- x$y >= min(ylim) & x$y <= max(ylim)
  xy_filter <- which(x_filter & y_filter)
  x <- x[xy_filter, , drop = FALSE]

  label(
    x = x$x,
    y = x$y,
    labels = x$label,
    type = type,
    col = x$col,
    # cex = x$cex,
    xpd = TRUE
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
#' @param active A [`logical`] scalar: should the active observations be
#'  plotted?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  plotted?
#' @param principal A [`logical`] scalar: should principal coordinates be
#'  returned? If `FALSE`, standard coordinates are returned.
#' @param extra_quali An optional vector of qualitative data for aesthetics
#'  mapping.
#' @param extra_quanti An optional vector of quantitative data for aesthetics
#'  mapping. If a single [`character`] string is passed, it must be one of
#'  "`observation`", "`mass`", "`sum`", "`contribution`" or "`cos2`"
#'  (see [`augment()`]).
#' @param color The colors for lines and points (will be mapped to
#'  `extra_quanti` or `extra_quali`; if both are set, the latter has priority).
#'  Ignored if set to `FALSE`. If `NULL`, the default color scheme will be used.
#' @param fill The background colors for points (will be mapped to
#'  `extra_quanti` or `extra_quali`; if both are set, the latter has priority).
#'  Ignored if set to `FALSE`.
#' @param symbol A vector of plotting characters or symbols (will be mapped to
#'  `extra_quali`). This can either be a single character or an integer code for
#'  one of a set of graphics symbols. If `symbol` is a named a named vector,
#'  then the symbols will be associated with their name within `extra_quali`.
#'  Ignored if set to `FALSE`.
#' @param size A length-two [`numeric`] vector giving range of possible sizes
#'  (greater than 0; will be mapped to `extra_quanti`).
#'  Ignored if set to `FALSE`.
#' @param line_type A specification for the line type (will be mapped to
#'  `extra_quali`). If `line_type` is a named a named vector, then the line
#'  types will be associated with their name within `extra_quali`.
#'  Ignored if set to `FALSE`.
#' @param line_width A specification for the line type and width (will
#'  be mapped to `extra_quanti`).
#'  Ignored if set to `FALSE`.
#' @param ... Further [graphical parameters][graphics::par].
#' @return
#'  A [`data.frame`] with the following columns:
#'    \describe{
#'     \item{`x`}{Coordinates along x.}
#'     \item{`y`}{Coordinates along y.}
#'     \item{`extra_quali`}{Extra qualitative variable to be highlighted.}
#'     \item{`extra_quanti`}{Extra quantitative variable to be highlighted.}
#'     \item{`label`}{Label.}
#'     \item{`sup`}{Is supplementary?}
#'     \item{`col`}{Color for lines and symbols.}
#'     \item{`bg`}{Background color for symbols.}
#'     \item{`pch`}{Symbols.}
#'     \item{`cex`}{Symbol sizes.}
#'     \item{`lty`}{Line types.}
#'     \item{`lwd`}{Line widths.}
#'    }
#' @author N. Frerebeau
#' @keywords internal
prepare_plot <- function(x, margin, ..., axes = c(1, 2), active = TRUE,
                         sup = TRUE, principal = TRUE,
                         extra_quali = NULL, extra_quanti = NULL,
                         color = NULL, fill = FALSE,
                         symbol = NULL, size = c(1, 6),
                         line_type = NULL, line_width = size) {
  ## Validation
  arkhe::assert_scalar(margin, "numeric")
  arkhe::assert_type(axes, "numeric")
  arkhe::assert_length(axes, 2)
  arkhe::assert_scalar(sup, "logical")
  arkhe::assert_scalar(principal, "logical")

  ## /!\ Backward compatibility /!\
  high <- list(...)$highlight
  if (length(high) == 1) {
    if (high == "observation") extra_quali <- high else extra_quanti <- high
  }

  ## Prepare data
  data <- augment(x, margin = margin, axes = axes, principal = principal)
  n <- nrow(data)

  ## Recode
  data$observation <- ifelse(data$supplementary, "suppl.", "active")

  ## Recycle graphical parameters if of length one
  dots <- list(...)
  col <- recycle(dots$col %||% graphics::par("col"), n)
  bg <- recycle(dots$bg %||% graphics::par("bg"), n)
  pch <- recycle(dots$pch %||% 16, n)
  cex <- recycle(dots$cex %||% graphics::par("cex"), n)
  lty <- recycle(dots$lty %||% graphics::par("lty"), n)
  lwd <- recycle(dots$lwd %||% graphics::par("lwd"), n)

  ## Highlight quantitative information
  if (length(extra_quanti) == 1) {
    choices <- c("mass", "sum", "contribution", "cos2")
    extra_quanti <- match.arg(extra_quanti, choices = choices, several.ok = FALSE)
    extra_quanti <- data[[extra_quanti]]
  }
  if (length(extra_quanti) > 0) {
    extra_quanti <- as.vector(extra_quanti)
    arkhe::assert_type(extra_quanti, "numeric")
    arkhe::assert_length(extra_quanti, n)
    ## Continuous scales
    if (!isFALSE(color)) col <- khroma::palette_color_continuous(colors = color)(extra_quanti)
    if (!isFALSE(fill)) bg <- khroma::palette_color_continuous(colors = fill)(extra_quanti)
    if (!isFALSE(size)) cex <- khroma::palette_size_range(range = size)(extra_quanti)
    if (!isFALSE(line_width)) lwd <- khroma::palette_size_range(range = line_width)(extra_quanti)
  } else {
    extra_quanti <- rep(NA_real_, n)
  }

  ## Highlight qualitative information
  if (is.null(extra_quali) && has_groups(x, margin = margin)) {
    extra_quali <- get_groups(x, margin = margin)
  }
  if (is.character(extra_quali) && length(extra_quali) == 1) {
    choices <- c("observation")
    extra_quali <- match.arg(extra_quali, choices = choices, several.ok = FALSE)
    extra_quali <- data[[extra_quali]]
  }
  if (!isFALSE(extra_quali) && length(extra_quali) > 0) {
    extra_quali <- as.vector(extra_quali)
    arkhe::assert_length(extra_quali, n)
    ## Discrete scales
    if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(extra_quali)
    if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(extra_quali)
    if (!isFALSE(symbol)) pch <- khroma::palette_shape(symbols = symbol)(extra_quali)
    if (!isFALSE(line_type)) lty <- khroma::palette_line(types = line_type)(extra_quali)
  } else {
    extra_quali <- rep(NA_character_, n)
  }

  coord <- data.frame(
    data,
    x = data[[1L]],
    y = data[[2L]],
    extra_quali = extra_quali,
    extra_quanti = extra_quanti,
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
#' @param x A [`data.frame`] returned by [prepare_plot()].
#' @param args A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL` or empty, no legend is displayed.
#' @param points A [`logical`] scalar: legend for points?
#' @param lines A [`logical`] scalar: legend for lines?
#' @author N. Frerebeau
#' @keywords internal
prepare_legend <- function(x, args, points = TRUE, lines = TRUE) {
  quanti <- x$extra_quanti
  quali <- x$extra_quali

  if (!is.list(args) || length(args) == 0) return(NULL)
  if (all(is.na(quanti)) && all(is.na(quali))) return(NULL)

  ## Continuous scale
  if (!all(is.na(quanti))) {
    quanti <- quanti[!is.na(quanti)]
    # im <- grDevices::as.raster(x$col)

    pr <- pretty(quanti, n = ifelse(nrow(x) > 5, 5, nrow(x)))
    pr <- pr[pr <= max(quanti) & pr >= min(quanti)]
    i <- order(quanti, method = "radix")[!duplicated(quanti)]

    col <- grDevices::colorRamp(x$col[i])(scale_range(pr, from = range(quanti)))
    col <- grDevices::rgb(col, maxColorValue = 255)

    leg <- list(legend = pr, col = col)
    if (points) {
      cex <- stats::approx(x = quanti[i], y = x$cex[i], xout = pr, ties = "ordered")$y
      leg <- utils::modifyList(leg, list(pch = unique(x$pch), pt.cex = cex))
    }
    if (lines) {
      lwd <- stats::approx(x = quanti[i], y = x$lwd[i], xout = pr, ties = "ordered")$y
      leg <- utils::modifyList(leg, list(lty = unique(x$lty), lwd = lwd))
    }
  }
  ## Discrete scale
  if (!all(is.na(quali))) {
    param <- stats::aggregate(
      x[, c("col", "bg", "pch", "lty")],
      by = list(leg = quali),
      FUN = unique
    )
    leg <- list(legend = param$leg, col = param$col)
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
