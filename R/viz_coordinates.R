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
                        labels = FALSE,
                        map_alpha = NULL, map_color = NULL,
                        map_shape = NULL, map_size = NULL,
                        scale_color = NULL, scale_shape = NULL,
                        scale_size = NULL,
                        main = NULL, sub = NULL, ...) {
    viz_points(x, margin = 1, axes = axes,
               active = active, sup = sup, labels = labels,
               map_alpha = map_alpha, map_color = map_color,
               map_shape = map_shape, map_size = map_size,
               scale_color = scale_color,
               scale_shape = scale_shape,
               scale_size = scale_size,
               main = main, sub = sub, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_individuals
#' @aliases viz_rows,BootstrapCA-method
setMethod(
  f = "viz_rows",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), scale_color = NULL,
                        scale_shape = NULL, ...) {
    group <- get_groups(x, margin = 1)
    viz_points(x, margin = 1, axes = axes,
               active = TRUE, sup = TRUE, labels = FALSE,
               map_color = group, scale_color = scale_color,
               map_shape = group, scale_shape = scale_shape, ...)
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
                        labels = FALSE, map_alpha = NULL, map_color = NULL,
                        map_shape = NULL, map_size = NULL,
                        scale_color = NULL, scale_shape = NULL,
                        scale_size = NULL, main = NULL, sub = NULL, ...) {
    viz_rows(x, axes = axes, active = active, sup = sup, labels = labels,
             map_alpha = map_alpha, map_color = map_color,
             map_shape = map_shape, map_size = map_size,
             scale_color = scale_color, scale_shape = scale_shape,
             scale_size = scale_size, main = main, sub = sub, ...)
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
                        labels = FALSE, map_alpha = NULL, map_color = NULL,
                        map_shape = NULL, map_size = NULL,
                        scale_color = NULL, scale_shape = NULL,
                        scale_size = NULL, main = NULL, sub = NULL, ...) {
    viz_points(x, margin = 2, axes = axes,
               active = active, sup = sup, labels = labels,
               map_alpha = map_alpha, map_color = map_color,
               map_shape = map_shape, map_size = map_size,
               scale_color = scale_color,
               scale_shape = scale_shape,
               scale_size = scale_size,
               main = main, sub = sub, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_columns,BootstrapCA-method
setMethod(
  f = "viz_columns",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), scale_color = NULL,
                        scale_shape = NULL, ...) {
    group <- get_groups(x, margin = 2)
    viz_points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, map_color = group, scale_color = scale_color,
               map_shape = group, scale_shape = scale_shape, ...)
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
                        labels = TRUE, map_alpha = NULL, map_color = NULL,
                        map_linetype = NULL, map_size = NULL,
                        scale_color = NULL, scale_linetype = NULL,
                        scale_size = NULL, main = NULL, sub = NULL, ...) {
    ## Prepare data
    coord <- prepare_coord(x, margin = 2, axes = axes, active = active,
                           sup = sup)

    ## Graphical parameters
    param <- prepare_param(coord, map_alpha = map_alpha,
                           map_color = map_color, scale_color = scale_color,
                           map_linetype = map_linetype, scale_linetype = scale_linetype,
                           map_size = map_size, scale_size = scale_size)

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    xlim <- if (is_scaled(x)) c(-1, 1) else range(coord$x)
    ylim <- if (is_scaled(x)) c(-1, 1) else range(coord$y)
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

    ## Evaluate pre-plot expressions
    # panel.first

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
      col = param$col, lty = param$lty, lwd = param$lwd
    )

    ## Labels
    if (labels && nrow(coord) > 1) {
      usr <- graphics::par("usr")
      viz_labels(x = coord$x, y = coord$y, labels = coord$label,
                 xlim = usr[c(1, 2)], ylim = usr[c(3, 4)], col = param$col)
    }

    ## Evaluate post-plot and pre-axis expressions
    # panel.last

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
  definition = function(x, axes = c(1, 2), scale_color = NULL,
                        scale_shape = NULL, ...) {
    group <- get_groups(x, margin = 2)
    viz_points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, map_color = group, scale_color = scale_color,
               map_shape = group, scale_shape = scale_shape, ...)
    invisible(x)
  }
)

# Helpers ======================================================================
viz_points <- function(x, margin, axes, active = TRUE, sup = TRUE, labels = FALSE,
                       map_alpha = NULL, map_color = NULL, scale_color = NULL,
                       map_shape = NULL, scale_shape = NULL,
                       map_size = NULL, scale_size = NULL,
                       main = NULL, sub = NULL, ...) {
  ## Prepare data
  coord <- prepare_coord(x, margin = margin, axes = axes, active = active,
                         sup = sup)

  ## Graphical parameters
  param <- prepare_param(coord, map_alpha = map_alpha,
                         map_color = map_color, scale_color = scale_color,
                         map_shape = map_shape, scale_shape = scale_shape,
                         map_size = map_size, scale_size = scale_size)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(coord$x)
  ylim <- range(coord$y)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  ## Evaluate pre-plot expressions
  # panel.first

  ## Plot
  graphics::abline(h = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  graphics::abline(v = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  graphics::points(x = coord$x, y = coord$y, col = param$col,
                   pch = param$pch, cex = param$cex)

  ## Labels
  if (labels) {
    usr <- graphics::par("usr")
    viz_labels(x = coord$x, y = coord$y, labels = coord$label,
               xlim = usr[c(1, 2)], ylim = usr[c(3, 4)], col = param$col, ...)
  }

  ## Evaluate post-plot and pre-axis expressions
  # panel.last

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

  invisible(param)
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
                          sup = TRUE, principal = TRUE, group = NULL) {
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

  data
}

prepare_param <- function(x, map_alpha = NULL,
                          map_color = NULL, scale_color = NULL,
                          map_size = NULL, scale_size = NULL,
                          map_linetype = NULL, scale_linetype = NULL,
                          map_shape = NULL, scale_shape = NULL) {
  n <- nrow(x)

  ## Graphical parameters
  col <- scale_color
  pch <- scale_shape
  lty <- scale_linetype
  cex <- scale_size %||% graphics::par("cex")
  lwd <- scale_size %||% graphics::par("lwd")

  choices <- c("observation", "mass", "sum", "contribution", "cos2")

  if (!is.null(map_color)) {
    if (length(map_color) == 1) {
      map_color <- match.arg(map_color, choices = choices)
      map_color <- x[[map_color]]
    }

    if (is.double(map_color)) {
      ## Continuous scale
      if (is.null(col))
        col <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
      map_color <- map_color / max(map_color)
      col <- grDevices::colorRamp(col)(map_color)
      col <- grDevices::rgb(col[, 1], col[, 2], col[, 3], maxColorValue = 255)
    } else {
      ## Discrete scale
      n_col <- length(unique(map_color))
      if (is.null(col)) col <- grDevices::hcl.colors(n_col, "viridis")
      if (length(col) < n_col) assert_length(col, n_col)
      col <- col[as.factor(map_color)]
    }
  } else {
    col <- rep(col %||% graphics::par("col"), n)
  }

  if (!is.null(map_alpha)) {
    if (length(map_alpha) == 1) {
      map_alpha <- match.arg(map_alpha, choices = choices[c(2, 3, 4, 5)])
      map_alpha <- x[[map_alpha]]
    }

    alpha <- as.numeric(map_alpha)
    alpha <- alpha / max(alpha)
    col <- grDevices::adjustcolor(col, alpha.f = alpha)
  }

  if (!is.null(map_linetype)) {
    if (length(map_linetype) == 1) {
      map_linetype <- match.arg(map_linetype, choices = choices[1])
      map_linetype <- x[[map_linetype]]
    }

    n_lty <- length(unique(map_linetype))
    if (is.null(lty)) lty <- seq_along(unique(map_linetype))
    lty <- lty[as.factor(map_linetype)]
  } else {
    lty <- rep(lty %||% graphics::par("lty"), n)
  }

  if (!is.null(map_shape)) {
    if (length(map_shape) == 1) {
      map_shape <- match.arg(map_shape, choices = choices[1])
      map_shape <- x[[map_shape]]
    }

    if (is.null(pch)) pch <- seq_along(unique(map_shape))
    pch <- pch[as.factor(map_shape)]
  } else {
    pch <- rep(pch %||% graphics::par("pch"), n)
  }

  if (!is.null(map_size)) {
    if (length(map_size) == 1) {
      map_size <- match.arg(map_size, choices = choices[c(2, 3, 4, 5)])
      map_size <- x[[map_size]]
    }

    cex <- cex + map_size / max(map_size)
    lwd <- lwd + map_size / max(map_size)
  } else {
    cex <- rep(cex, n)
    lwd <- rep(lwd, n)
  }

  data.frame(col = col, pch = pch, cex = cex, lty = lty, lwd = lwd)
}
