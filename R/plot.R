# PLOT
#' @include AllGenerics.R
NULL

# Coordinates ==================================================================
## Rows ------------------------------------------------------------------------
#' @export
#' @rdname viz_coordinates
#' @aliases viz_rows,MultivariateAnalysis-method
setMethod(
  f = "viz_rows",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, alpha = NULL, colour = NULL,
                        shape = NULL, size = NULL, ...) {
    .points(x, margin = 1, axes = axes, active = active, sup = sup,
            labels = labels, alpha = alpha, colour = colour,
            shape = shape, size = size, ...)
    invisible(x)
  }
)

#' @export
#' @rdname viz_coordinates
#' @aliases viz_rows,BootstrapCA-method
setMethod(
  f = "viz_rows",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 1)
    .points(x, margin = 1, axes = axes, active = TRUE, sup = TRUE,
            labels = FALSE, alpha = NULL, colour = group,
            shape = group, size = NULL, ...)
    invisible(x)
  }
)

## Individuals -----------------------------------------------------------------
#' @export
#' @rdname viz_coordinates
#' @aliases viz_individuals,PCA-method
setMethod(
  f = "viz_individuals",
  signature = c(x = "PCA"),
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, alpha = NULL, colour = NULL,
                        shape = NULL, size = NULL, ...) {
    viz_rows(x, axes = axes, active = active, sup = sup,
             labels = labels, alpha = alpha, colour = colour,
             shape = shape, size = size, ...)

    invisible(x)
  }
)

## Columns ---------------------------------------------------------------------
#' @export
#' @rdname viz_coordinates
#' @aliases viz_columns,MultivariateAnalysis-method
setMethod(
  f = "viz_columns",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = FALSE, alpha = NULL, colour = NULL,
                        shape = NULL, size = NULL, ...) {
    .points(x, margin = 2, axes = axes, active = active, sup = sup,
            labels = labels, alpha = alpha, colour = colour,
            shape = shape, size = size, ...)

    invisible(x)
  }
)

#' @export
#' @rdname viz_coordinates
#' @aliases viz_columns,BootstrapCA-method
setMethod(
  f = "viz_columns",
  signature = c(x = "BootstrapCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 2)
    .points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
            labels = FALSE, alpha = NULL, colour = group,
            shape = group, size = NULL, ...)

    invisible(x)
  }
)

## Arrows ----------------------------------------------------------------------
#' @export
#' @rdname viz_coordinates
#' @aliases viz_variables,PCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "PCA"),
  definition = function(x, axes = c(1, 2), active = TRUE, sup = TRUE,
                        labels = TRUE, alpha = NULL, colour = NULL,
                        linetype = NULL, size = NULL,
                        main = NULL, sub = NULL, ...) {
    ## Prepare data
    coord <- prepare_coord(x, margin = 2, axes = axes, active = active,
                           sup = sup)

    ## Graphical parameters
    param <- prepare_param(coord, alpha = alpha, colour = colour,
                           linetype = linetype, size = size, ...)

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
      .labels(x = coord$x, y = coord$y, labels = coord$label,
              xlim = xlim, ylim = ylim, col = param$col)
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
#' @rdname viz_coordinates
#' @aliases viz_variables,BootstrapPCA-method
setMethod(
  f = "viz_variables",
  signature = c(x = "BootstrapPCA"),
  definition = function(x, axes = c(1, 2), ...) {
    group <- get_groups(x, margin = 2)
    .points(x, margin = 2, axes = axes, active = TRUE, sup = TRUE,
            labels = FALSE, alpha = NULL, colour = group,
            shape = group, size = NULL, ...)

    invisible(x)
  }
)

# Ellipses =====================================================================
#' @export
#' @rdname viz_wrap
#' @aliases viz_hull,MultivariateAnalysis-method
setMethod(
  f = "viz_hull",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL, ...) {
    hull <- wrap_hull(x, margin = margin, axes = axes, group = group)
    n <- length(hull)

    ## Graphical parameters
    border <- list(...)$border %||% graphics::par("col")
    col <- list(...)$col %||% NA
    lty <- list(...)$lty %||% graphics::par("lty")
    lwd <- list(...)$lwd %||% graphics::par("lwd")
    if (length(border) != n) border <- rep(border, length.out = n)
    if (length(col) != n) col <- rep(col, length.out = n)
    if (length(lty) != n) lty <- rep(lty, length.out = n)
    if (length(lwd) != n) lwd <- rep(lwd, length.out = n)

    for (i in seq_along(hull)) {
      graphics::polygon(x = hull[[i]], border = border[i],
                        col = col[i], lty = lty[i], lwd = lwd[i])
    }

    invisible(x)
  }
)

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

# Contributions ================================================================
#' @export
#' @rdname viz_contributions
#' @aliases viz_contributions,MultivariateAnalysis-method
setMethod(
  f = "viz_contributions",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 2, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        horiz = FALSE, col = "grey90", border = "grey10", ...) {
    ## Prepare data
    data <- prepare_contrib(x, margin = margin, axes = axes, sort = sort,
                            decreasing = decreasing, limit = limit)

    ylab <- sprintf("Contributions to %s (%%)",
                    paste0("F", axes, collapse = "-"))

    ## Bar plot
    mid <- graphics::barplot(
      height = data$y,
      names.arg = data$x,
      horiz = horiz,
      xlab = if (horiz) ylab else NULL,
      ylab = if (horiz) NULL else ylab,
      col = col,
      border = border,
      las = 1,
      ...
    )

    invisible(x)
  }
)

# Must return a data.frame (`x`, `y`, `label`)
prepare_contrib <- function(object, margin, axes, sort = TRUE,
                            decreasing = TRUE, limit = 10) {
  ## Get data
  contrib <- get_contributions(object, margin = margin)
  if (length(axes) > 1) {
    values <- joint_contributions(object, margin = margin, axes = axes)
  } else {
    values <- contrib[[axes[[1]]]]
  }

  ## Prepare data
  data <- data.frame(
    x = rownames(contrib),
    y = values,
    label = round(values, digits = 2)
  )

  ## Sort data
  if (sort) {
    data <- data[order(data$y, decreasing = decreasing), ]
  }

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ## Prevent reordering by ggplot2
  data$x <- factor(data$x, levels = unique(data$x))

  data
}

# Cos2 =========================================================================
#' @export
#' @rdname viz_contributions
#' @aliases viz_cos2,MultivariateAnalysis-method
setMethod(
  f = "viz_cos2",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 2, axes = c(1, 2), active = TRUE,
                        sup = TRUE, sort = TRUE, decreasing = TRUE,
                        limit = 10, horiz = FALSE,
                        col = "grey90", border = "grey10", ...) {
    ## Prepare data
    data <- prepare_cos2(x, margin = margin, axes = axes,
                         active = active, sup = sup, sort = sort,
                         decreasing = decreasing, limit = limit)

    xx <- sprintf("along %s", paste0("F", axes, collapse = "-"))
    ylab <- bquote(paste(plain(cos)^2~.(xx)))

    ## Bar plot
    mid <- graphics::barplot(
      height = data$y,
      names.arg = data$x,
      horiz = horiz,
      xlab = if (horiz) ylab else NULL,
      ylab = if (horiz) NULL else ylab,
      col = col,
      border = border,
      las = 1,
      ...
    )

    invisible(x)
  }
)

# Must return a data.frame (`x`, `y`, `label`)
prepare_cos2 <- function(object, margin, axes, active = TRUE, sup = TRUE,
                         sort = TRUE, decreasing = TRUE, limit = 10) {
  ## Get data
  cos2 <- get_cos2(object, margin = margin)
  if (length(axes) > 1) {
    values <- joint_cos2(object, margin = margin, axes = axes)
  } else {
    values <- cos2[[axes[[1]]]]
  }

  ## Prepare data
  data <- data.frame(
    x = rownames(cos2),
    y = values,
    label = round(values, digits = 2)
  )

  ## Subset
  if (!active & sup) data <- data[cos2$.sup, ]
  if (active & !sup) data <- data[!cos2$.sup, ]

  ## Sort data
  if (sort) {
    data <- data[order(data$y, decreasing = decreasing), ]
  }

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ## Prevent reordering by ggplot2
  data$x <- factor(data$x, levels = unique(data$x))

  data
}

# Helpers ======================================================================
.points <- function(x, margin, axes, active = TRUE, sup = TRUE,
                    labels = FALSE, alpha = NULL, colour = NULL,
                    shape = NULL, size = NULL,
                    main = NULL, sub = NULL, ...) {
  ## Prepare data
  coord <- prepare_coord(x, margin = margin, axes = axes, active = active,
                         sup = sup)

  ## Graphical parameters
  param <- prepare_param(coord, alpha = alpha, colour = colour,
                         shape = shape, size = size, ...)

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
    .labels(x = coord$x, y = coord$y, labels = coord$label,
            xlim = xlim, ylim = ylim, col = param$col, ...)
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

roundrect <- function(xleft, ybottom, xright, ytop,
                      rounding = 0.25, n = 200, ...) {

  XD <- YD <- min(c(xright - xleft, ytop - ybottom))
  xi <- rounding * XD
  yi <- rounding * YD

  ## Elliptic corners function
  elx <- function(from, to) xi * cos(seq(from, to, length.out = n / 4))
  ely <- function(from, to) yi * sin(seq(from, to, length.out = n / 4))

  ## x and y coordinates
  xc <- c(xright - xi + elx(0, pi / 2),
          xleft + xi + elx(pi / 2, pi),
          xleft + xi + elx(pi, 3 * pi / 2),
          xright - xi + elx(3 * pi / 2, 2 * pi))
  yc <- c(ytop - yi + ely(0, pi / 2),
          ytop - yi + ely(pi / 2, pi),
          ybottom + yi + ely(pi, 3 * pi / 2),
          ybottom + yi + ely(3 * pi / 2, 2 * pi))

  graphics::polygon(x = xc, y = yc, ...)
}

shadowtext <- function(x, y, labels,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"), bg = graphics::par("bg"),
                       theta= seq(0, 2 * pi, length.out = 50), r = 0.1, ... ) {

  xo <- r * graphics::strwidth("A", cex = cex, ...)
  yo <- r * graphics::strheight("A", cex = cex, ...)

  for (i in theta) {
    graphics::text(x + cos(i) * xo, y + sin(i) * yo, labels, col = bg, ...)
  }

  graphics::text(x, y, labels, col = col, ...)
}

.labels <- function(x, y, labels, xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                    box = FALSE, segment = TRUE,
                    cex = graphics::par("cex"), col = graphics::par("fg"),
                    bg = graphics::par("bg"), ...) {
  ## Compute label positions
  lay <- wordcloud::wordlayout(x = x, y = y, words = labels,
                               xlim = xlim, ylim = ylim, cex = cex, ...)
  xt <- lay[, "x"]
  yt <- lay[, "y"]
  wt <- lay[, "width"]
  ht <- lay[, "ht"]

  mar_x <- graphics::strwidth ("m", cex = cex, ...) * 0.3
  mar_y <- graphics::strheight("x", cex = cex, ...) * 0.3

  ## Shift label positions
  xt <- xt + max(wt)
  yt <- yt + max(ht)

  ## Plot lines
  if (isTRUE(segment)) {
    for (i in 1:length(x)) {
      if (x[i] != xt[i] || y[i] != yt[i]) {
        graphics::lines(
          x = c(x[i], xt[i] - 0.5 * wt[i]),
          y = c(y[i], yt[i]),
          col = "grey"
        )
      }
    }
  }

  ## Plot boxes
  if (isTRUE(box)) {
    .mapply(
      FUN = function(x, y, w, h, col, border) {
        roundrect(
          xleft = x - w - mar_x,
          ybottom = y - h - mar_y,
          xright = x + w + mar_x,
          ytop = y + h + mar_y,
          col = col,
          border = border
        )
      },
      dots = list(x = xt, y = yt, w = wt * 0.5, h = ht * 0.5,
                  col = bg, border = col),
      MoreArgs = NULL
    )
  } else {
    shadowtext(x = xt, y = yt, labels = labels, col = col, bg = bg, cex = cex)
  }

  ## Plot labels
  graphics::text(x = xt, y = yt, labels = labels, col = col, cex = cex)
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
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

prepare_param <- function(x, alpha = NULL, colour = NULL, size = NULL,
                          linetype = NULL, shape = NULL, ...) {
  n <- nrow(x)

  ## Graphical parameters
  pch <- list(...)$pch
  lty <- list(...)$lty
  col <- list(...)$col
  cex <- list(...)$cex %||% graphics::par("cex")
  lwd <- list(...)$lwd %||% graphics::par("lwd")

  choices <- c("observation", "mass", "sum", "contribution", "cos2")

  if (!is.null(colour)) {
    if (length(colour) == 1) {
      colour <- match.arg(colour, choices = choices)
      colour <- x[[colour]]
    }

    if (is.double(colour)) {
      ## Continuous scale
      if (is.null(col)) col <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
      colour <- colour / max(colour)
      col <- grDevices::colorRamp(col)(colour)
      col <- grDevices::rgb(col[, 1], col[, 2], col[, 3], maxColorValue = 255)
    } else {
      ## Discrete scale
      n_col <- length(unique(colour))
      if (is.null(col)) col <- grDevices::rainbow(n_col) # Default
      if (length(col) < n_col) assert_length(col, n_col)
      col <- col[as.factor(colour)]
    }
  } else {
    col <- rep(col %||% graphics::par("col"), n)
  }

  if (!is.null(alpha)) {
    if (length(alpha) == 1) {
      alpha <- match.arg(alpha, choices = choices[c(2, 3, 4, 5)])
      alpha <- x[[alpha]]
    } else {
      alpha <- as.numeric(alpha)
    }

    alpha <- alpha / max(alpha)
    col <- grDevices::adjustcolor(col, alpha.f = alpha)
  }

  if (!is.null(linetype)) {
    if (length(shape) == 1) {
      linetype <- match.arg(linetype, choices = choices[1])
      linetype <- x[[linetype]]
    }

    n_lty <- length(unique(colour))
    if (is.null(lty)) lty <- seq_along(unique(linetype))
    lty <- lty[as.factor(linetype)]
  } else {
    lty <- rep(lty %||% graphics::par("lty"), n)
  }

  if (!is.null(shape)) {
    if (length(shape) == 1) {
      shape <- match.arg(shape, choices = choices[1])
      shape <- x[[shape]]
    }

    if (is.null(pch)) pch <- seq_along(unique(shape))
    pch <- pch[as.factor(shape)]
  } else {
    pch <- rep(pch %||% graphics::par("pch"), n)
  }

  if (!is.null(size)) {
    if (length(size) == 1) {
      size <- match.arg(size, choices = choices[c(2, 3, 4, 5)])
      size <- x[[size]]
    }

    cex <- cex + size / max(size)
    lwd <- lwd + size / max(size)
  } else {
    cex <- rep(cex, n)
    lwd <- rep(lwd, n)
  }

  data.frame(col = col, pch = pch, cex = cex, lty = lty, lwd = lwd)
}
