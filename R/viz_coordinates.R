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
                        labels = FALSE, highlight = NULL,
                        color = NULL, symbol = 16, size = c(1, 3),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 1, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               color = color, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_individuals
#' @aliases viz_rows,BootstrapCA-method
setMethod(
  f = "viz_rows",
  signature = c(x = "BootstrapCA"),
  definition = function(x, ..., axes = c(1, 2)) {
    group <- get_groups(x, margin = 1)
    viz_points(x, ..., margin = 1, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group)
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
                        labels = FALSE, highlight = NULL,
                        color = NULL, symbol = 16, size = c(1, 3),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 1, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               color = color, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend, ...)
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
                        labels = FALSE, highlight = NULL,
                        color = NULL, symbol = 16, size = c(1, 3),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 2, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               color = color, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_columns,MultivariateBootstrap-method
setMethod(
  f = "viz_columns",
  signature = c(x = "MultivariateBootstrap"),
  definition = function(x, ..., axes = c(1, 2)) {
    group <- get_groups(x, margin = 2)
    viz_points(x, ..., margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group)
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
                        labels = list(how = "contribution", n = 10),
                        highlight = NULL,
                        color = NULL, symbol = NULL, size = 1,
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    ## Prepare data
    coord <- prepare(x, margin = 2, axes = axes, active = active,
                     sup = sup, highlight = highlight,
                     color = color, line_type = symbol,
                     line_width = size, ...)

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
      viz_labels(coord, select = labels$how, n = labels$n)
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
                        labels = FALSE, highlight = NULL,
                        color = NULL, symbol = 16, size = c(1, 3),
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, margin = 2, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               color = color, symbol = symbol, size = size,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend, ...)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_variables,BootstrapPCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "BootstrapPCA"),
  definition = function(x, ..., axes = c(1, 2)) {
    group <- get_groups(x, margin = 2)
    viz_points(x, ..., margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group)
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
#'  `list` of two named elements, `how` (a string specifying how to select the
#'  labels to be drawn) and `n` (an integer specifying the number of labels to
#'  be drawx). See examples below.
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
#' @param symbol A vector of plotting characters or symbols.
#' @inheritParams prepare
#' @author N. Frerebeau
#' @keywords internal
viz_points <- function(x, margin, axes, ...,
                       active = TRUE, sup = TRUE,
                       labels = list(how = "contribution", n = 10),
                       highlight = NULL, color = NULL,
                       symbol = 16, size = c(1, 3),
                       xlim = NULL, ylim = NULL,
                       main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                       ann = graphics::par("ann"), frame.plot = TRUE,
                       panel.first = NULL, panel.last = NULL,
                       legend = list(x = "topleft")) {
  ## Prepare data
  coord <- prepare(x, margin = margin, axes = axes, active = active,
                   sup = sup, highlight = highlight,
                   color = color, shape = symbol, size = size, ...)

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
    viz_labels(coord, select = labels$how, n = labels$n)
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

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

#' Non-Overlapping Text Labels
#'
#' @param x A [`data.frame`] (typically returned by [prepare()]).
#' @param select A [`character`] string specifying the variable to select
#'  labels. If `NULL`, all labels are drawn.
#' @param n An [`integer`] specifying the number of labels to draw.
#'  Only the labels of the top \eqn{n} observations according to `select` will
#'  be drawn. If `NULL`, all labels are drawn.
#' @param type A [`character`] string specifying the shape of the field.
#'  It must be one of "`text`", "`shadow`" or "`box`". Any unambiguous substring
#'  can be given.
#' @param ... Currently not used.
#' @author N. Frerebeau
#' @keywords internal
viz_labels <- function(x, select = "contribution", n = 10,
                       type = "shadow", ...) {
  ## Select
  if (!is.null(select) && !is.null(n) && n > 0) {
    top <- min(nrow(x), n)
    how <- x[[select]]
    k <- order(how, decreasing = TRUE)[seq_len(top)] # Get order
    x <- x[k, , drop = FALSE] # Subset
  }

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
