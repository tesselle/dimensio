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
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, highlight = NULL,
                        main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL, ...) {
    viz_points(x, margin = 1, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_individuals
#' @aliases viz_rows,BootstrapCA-method
setMethod(
  f = "viz_rows",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 1)
    viz_points(x, margin = 1, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group, ...)
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
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, highlight = NULL,
                        main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL, ...) {
    viz_rows(x, axes = axes, active = active, sup = sup, labels = labels,
             highlight = highlight, main = main, sub = sub,
             panel.first = panel.first, panel.last = panel.last, ...)
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
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, highlight = NULL,
                        main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL, ...) {
    viz_points(x, margin = 2, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_columns,BootstrapCA-method
setMethod(
  f = "viz_columns",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 2)
    viz_points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group, ...)
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
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = TRUE, highlight = NULL,
                        main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL, ...) {
    ## Prepare data
    coord <- prepare_coord(x, margin = 2, axes = axes, active = active,
                           sup = sup, highlight = highlight, ...)

    ## Save and restore graphical parameters
    ## pty: square plotting region, independent of device size
    old_par <- graphics::par(pty = "s", no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- range(coord$x, na.rm = TRUE, finite = TRUE)
    if (is_scaled(x)) xlim <- c(-1, 1)
    ylim <- range(coord$y, na.rm = TRUE, finite = TRUE)
    if (is_scaled(x)) ylim <- c(-1, 1)
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    graphics::abline(h = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
    graphics::abline(v = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))

    ## Scaled variables?
    if (is_scaled(x)) {
      plot_circle(x = 0, y = 0, radius = 1, n = 100, lwd = 1,
                  border = graphics::par("fg"))
    }

    graphics::arrows(
      x0 = 0, y0 = 0, x1 = coord$x, y1 = coord$y, length = 0.15, angle = 30,
      col = coord$col,
      lty = coord$lty,
      lwd = coord$lwd
    )

    ## Labels
    if (labels && nrow(coord) > 1) {
      viz_labels(x = coord$x, y = coord$y, labels = coord$label,
                 col = coord$col, cex = coord$cex)
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

    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_variables,BootstrapPCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "BootstrapPCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 2)
    viz_points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group, ...)
    invisible(x)
  }
)

# Helpers ======================================================================
viz_points <- function(x, margin, axes, active = TRUE, sup = TRUE, labels = FALSE,
                       highlight = NULL, main = NULL, sub = NULL,
                       xlab = NULL, ylab = NULL, ann = graphics::par("ann"),
                       frame.plot = TRUE,
                       panel.first = NULL, panel.last = NULL, ...) {
  ## Prepare data
  coord <- prepare_coord(x, margin = margin, axes = axes, active = active,
                         sup = sup, highlight = highlight, ...)

  ## Save and restore graphical parameters
  ## pty: square plotting region, independent of device size
  old_par <- graphics::par(pty = "s", no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(coord$x, na.rm = TRUE, finite = TRUE)
  ylim <- range(coord$y, na.rm = TRUE, finite = TRUE)
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
  if (labels) {
    viz_labels(x = coord$x, y = coord$y, labels = coord$label,
               col = coord$col, cex = coord$cex)
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

  invisible(coord)
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

#' Draw a Circle
#'
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @examples
#' \dontrun{
#' plot(NA, xlim = c(-1, 1), ylim = c(-1, 1),
#'      axes = FALSE, ann = FALSE, asp = 1)
#' plot_circle(0, 0, 0.5)
#' }
#' @keywords internal
#' @author N. Frerebeau
#' @noRd
plot_circle <- function(x, y, radius, n = 100, ...) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

# Returns a [`data.frame`] with the following columns:
#   * `label`, `supplementary`, `mass`, `sum`, `contribution`, `cos2`,
#   * `x`, `y`, `group`, `data`, `observation`
prepare_coord <- function(object, margin, axes = c(1, 2), active = TRUE,
                          sup = TRUE, principal = TRUE, group = NULL,
                          highlight = NULL, ...) {
  ## Prepare data
  data <- augment(object, margin = margin, axes = axes, principal = principal)
  data$x <- data[[1]]
  data$y <- data[[2]]

  k <- get_order(object, margin = margin)
  if (!is.null(group)) {
    assert_length(group, nrow(data))
    group <- group[k]
  } else if (has_groups(object, margin = margin)) {
    group <- get_groups(object, margin = margin)
  } else {
    group <- rep(NA_character_, length(k))
  }
  data$group <- group

  type <- ifelse(margin == 1, "row", "column")
  data$data <- rep(type, length(k))

  ## Subset
  if (active & !sup) data <- data[!data$supplementary, ]
  if (!active & sup) data <- data[data$supplementary, ]
  data$observation <- ifelse(data$supplementary, "suppl.", "active")

  ## Graphical parameters
  param <- prepare_param(data, highlight = highlight, ...)

  cbind(data, param)
}

prepare_param <- function(x, highlight = NULL, alpha = FALSE, ...) {
  ## Graphical parameters
  col <- list(...)$col
  bg <- list(...)$bg
  pch <- list(...)$pch
  cex <- list(...)$cex
  lty <- list(...)$lty
  lwd <- list(...)$lwd

  n <- nrow(x)
  if (length(highlight) == 1) {
    choices <- c("mass", "sum", "contribution", "cos2", "observation")
    highlight <- match.arg(highlight, choices = choices, several.ok = FALSE)
    highlight <- x[[highlight]]
  }
  if (length(highlight) > 1) assert_length(highlight, n)

  ## Colors
  col <- scale_color(x = highlight, col = col, alpha = alpha)
  bg <- scale_color(x = highlight, col = bg, alpha = alpha)

  ## Symbol
  pch <- scale_symbole(x = highlight, symb = pch)

  ## Size
  cex <- scale_size(x = highlight, size = cex)

  ## Line type
  lty <- scale_symbole(x = highlight, symb = lty)

  ## Line width
  lwd <- scale_size(x = highlight, size = lwd)

  param <- data.frame(
    col = col,
    bg = bg,
    pch = pch,
    cex = cex,
    lty = lty,
    lwd = lwd
  )
  if (nrow(param) > n) param <- param[seq_len(n), , drop = FALSE]
  param
}

scale_color <- function(x, col = NULL, alpha = FALSE) {
  if (is.null(x)) {
    if (is.null(col)) col <- graphics::par("col")
    return(col)
  }

  if (is.double(x)) {
    ## Continuous scale
    if (is.null(col)) col <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
    x <- scale_range(x) # Rescale to [0,1]
    col <- grDevices::colorRamp(col)(x)
    col <- grDevices::rgb(col[, 1], col[, 2], col[, 3], maxColorValue = 255)
    ## Alpha transparency
    if (alpha) col <- grDevices::adjustcolor(col, alpha.f = alpha)
  } else {
    ## Discrete scale
    n_col <- length(unique(x))
    if (is.null(col)) col <- grDevices::hcl.colors(n_col, "viridis")
    col <- recycle(col, n_col)
    col <- col[as.factor(x)]
  }

  col
}
scale_symbole <- function(x, symb = NULL, what = "pch") {
  if (is.double(x) && length(symb) > 1) {
    warning("Continuous value supplied to discrete scale.", call. = FALSE)
  }

  if (is.null(symb)) symb <- graphics::par(what)
  if (is.null(x)) return(symb)

  n_symb <- length(unique(x))
  symb <- recycle(symb, n_symb)
  symb <- symb[as.factor(x)]
  symb
}
scale_size <- function(x, size = NULL, what = "cex") {
  if (is.null(size)) size <- graphics::par(what)
  if (!is.double(x)) {
    if (length(size) > 1)
      warning("Discrete value supplied to continuous scale.", call. = FALSE)
    return(size)
  }

  scale_range(x, to = range(size))
}
