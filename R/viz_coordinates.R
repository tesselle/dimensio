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
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, ..., margin = 1, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
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
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_rows(x, ..., axes = axes, active = active, sup = sup, labels = labels,
             highlight = highlight, xlim = xlim, ylim = ylim,
             main = main, sub = sub, panel.first = panel.first,
             panel.last = panel.last, legend = legend)
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
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_points(x, ..., margin = 2, axes = axes, active = active, sup = sup,
               labels = labels, highlight = highlight,
               xlim = xlim, ylim = ylim, main = main, sub = sub,
               panel.first = panel.first, panel.last = panel.last,
               legend = legend)
    invisible(x)
  }
)

#' @export
#' @rdname viz_variables
#' @aliases viz_columns,BootstrapCA-method
setMethod(
  f = "viz_columns",
  signature = c(x = "BootstrapCA"),
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
                        labels = TRUE, highlight = NULL,
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
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
      arkhe::circle(x = 0, y = 0, radius = 1, lwd = 1,
                    border = graphics::par("fg"), n = 100)
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
                        xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                        panel.first = NULL, panel.last = NULL,
                        legend = list(x = "topleft")) {
    viz_columns(x, ..., axes = axes, active = active, sup = sup,
                labels = labels, highlight = highlight,
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
  definition = function(x, ..., axes = c(1, 2)) {
    group <- get_groups(x, margin = 2)
    viz_points(x, ..., margin = 2, axes = axes, active = TRUE, sup = TRUE,
               labels = FALSE, highlight = group)
    invisible(x)
  }
)

# Helpers ======================================================================
viz_points <- function(x, margin, axes, ...,
                       active = TRUE, sup = TRUE, labels = FALSE,
                       highlight = NULL, xlim = NULL, ylim = NULL,
                       main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                       ann = graphics::par("ann"),frame.plot = TRUE,
                       panel.first = NULL, panel.last = NULL,
                       legend = list(x = "topleft")) {
  ## Prepare data
  coord <- prepare_coord(x, ..., margin = margin, axes = axes, active = active,
                         sup = sup, highlight = highlight)

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

  ## Legend
  prepare_legend(coord, legend, points = TRUE, lines = FALSE)

  invisible(coord)
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

# Returns a [`data.frame`] with the following columns:
#   * `label`, `supplementary`, `mass`, `sum`, `contribution`, `cos2`,
#   * `x`, `y`, `group`, `data`, `observation`
prepare_coord <- function(object, margin, ..., axes = c(1, 2), active = TRUE,
                          sup = TRUE, principal = TRUE, group = NULL,
                          highlight = NULL) {
  ## Prepare data
  data <- augment(object, margin = margin, axes = axes, principal = principal)
  data$x <- data[[1]]
  data$y <- data[[2]]

  k <- get_order(object, margin = margin)
  if (!is.null(group)) {
    arkhe::assert_length(group, nrow(data))
    group <- group[k]
  } else if (has_groups(object, margin = margin)) {
    group <- get_groups(object, margin = margin)
  } else {
    group <- rep(NA_character_, nrow(data))
  }
  data$group <- group

  type <- ifelse(margin == 1, "row", "column")
  data$data <- rep(type, nrow(data))

  ## Subset
  if (active & !sup) data <- data[!data$supplementary, ]
  if (!active & sup) data <- data[data$supplementary, ]
  data$observation <- ifelse(data$supplementary, "suppl.", "active")

  ## Graphical parameters
  param <- prepare_param(data, highlight = highlight, ...)

  cbind(data, param)
}

prepare_param <- function(x, ..., highlight = NULL, alpha = FALSE) {
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
  if (length(highlight) > 1) arkhe::assert_length(highlight, n)

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
  param$highlight <- highlight

  param
}

#' @param x A [`data.frame`] returned by [prepare_coord()].
#' @param args A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param points A [`logical`] scalar. Legend for points?
#' @param lines A [`logical`] scalar. Legend for lines?
#' @keywords internal
#' @noRd
prepare_legend <- function(x, args, points = TRUE, lines = TRUE) {
  h <- x$highlight

  if (!is.null(h) && is.list(args) && length(args) > 0) {
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
      leg <- list(legend = unique(h), col = unique(x$col))
      if (points) {
        leg <- utils::modifyList(leg, list(pt.bg = unique(x$bg),
                                           pch = unique(x$pch)))
      }
      if (lines) {
        leg <- utils::modifyList(leg, list(lty = unique(x$lty)))
      }
    }

    leg <- utils::modifyList(leg, args)
    do.call(graphics::legend, args = leg)
  }
}

scale_color <- function(x, col = NULL, alpha = FALSE) {
  if (is.null(x)) {
    if (is.null(col)) col <- graphics::par("col")
    return(col)
  }

  if (is.double(x)) {
    ## Continuous scale
    if (is.null(col)) col <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
    x <- arkhe::scale_range(x) # Rescale to [0,1]
    col <- grDevices::colorRamp(col)(x)
    col <- grDevices::rgb(col, maxColorValue = 255)
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

  arkhe::scale_range(x, to = range(size))
}
