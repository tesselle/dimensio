# BIPLOT
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @rdname biplot
#' @aliases biplot,CA-method
setMethod(
  f = "biplot",
  signature = c(x = "CA"),
  definition = function(x, ..., axes = c(1, 2),
                        type = c("rows", "columns", "contributions"),
                        active = TRUE, sup = TRUE,
                        labels = "columns",
                        col.rows = "#004488", col.columns = "#BB5566",
                        cex.rows = graphics::par("cex"),
                        cex.columns = graphics::par("cex"),
                        pch.rows = 16, pch.columns = 17,
                        xlim = NULL, ylim = NULL,
                        main = NULL, sub = NULL) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Type of biplot
    if (type == "rows") {
      princ_row <- TRUE
      princ_col <- FALSE
    }
    if (type == "columns") {
      princ_row <- FALSE
      princ_col <- TRUE
    }
    if (type == "contributions") {
      princ_row <- FALSE
      princ_col <- TRUE
    }

    ## Get data
    coord_row <-  prepare_coord(x, margin = 1, axes = axes,
                                active = active, sup = sup,
                                principal = princ_row)
    coord_col <-  prepare_coord(x, margin = 2, axes = axes,
                                active = active, sup = sup,
                                principal = princ_col)

    ## Graphical parameters
    if (type == "contributions") {
      coord_row$x <- coord_row$x * sqrt(coord_row$mass)
      coord_row$y <- coord_row$y * sqrt(coord_row$mass)

      cex.rows <- cex.rows + (coord_row$mass / max(coord_row$mass))
      cex.columns <- cex.columns + (coord_col$mass / max(coord_col$mass))
    }

    viz_biplot(
      coord_row, coord_col,
      ...,
      rows = TRUE, columns = TRUE, labels = labels,
      col.rows = col.rows, col.columns = col.columns,
      pch.rows = pch.rows, pch.columns = pch.columns,
      cex.rows = cex.rows,
      cex.columns = cex.columns,
      xlim = xlim, ylim = ylim,
      main = main, sub = sub,
      xlab = print_variance(x, axes[[1]]),
      ylab = print_variance(x, axes[[2]])
    )

    invisible(x)
  }
)

# PCA ==========================================================================
#' @export
#' @rdname biplot
#' @aliases biplot,PCA-method
setMethod(
  f = "biplot",
  signature = c(x = "PCA"),
  definition = function(x, ..., axes = c(1, 2), type = c("form", "covariance"),
                        active = TRUE, sup = TRUE,
                        labels = "variables",
                        col.rows = "#004488", col.columns = "#BB5566",
                        pch.rows = 16, pch.columns = 17,
                        lty = "solid", lwd = 2,
                        xlim = NULL, ylim = NULL,
                        main = NULL, sub = NULL) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Type of biplot
    if (type == "form") {
      princ_row <- TRUE
      princ_col <- FALSE
    }
    if (type == "covariance") {
      princ_row <- FALSE
      princ_col <- TRUE
    }

    ## Get data
    coord_row <-  prepare_coord(x, margin = 1, axes = axes,
                                active = active, sup = sup,
                                principal = princ_row)
    coord_col <-  prepare_coord(x, margin = 2, axes = axes,
                                active = active, sup = sup,
                                principal = princ_col)

    arrows_col <- function() {
      graphics::arrows(
        x0 = 0, y0 = 0,
        x1 = coord_col$x, y1 = coord_col$y,
        length = 0.10, angle = 30,
        col = col.columns, lty = lty, lwd = lwd
      )
    }

    viz_biplot(
      coord_row, coord_col,
      ...,
      rows = TRUE, columns = FALSE, labels = labels,
      col.rows = col.rows, col.columns = col.columns,
      pch.rows = pch.rows, pch.columns = pch.columns,
      xlim = xlim, ylim = ylim,
      main = main, sub = sub,
      xlab = print_variance(x, axes[[1]]),
      ylab = print_variance(x, axes[[2]]),
      panel.first = arrows_col(),
      panel.last = NULL
    )

    invisible(x)
  }
)

viz_biplot <- function(coord_row, coord_col, ..., rows = TRUE, columns = TRUE,
                       labels = c("rows", "columns", "individuals", "variables"),
                       col.rows = "#004488", col.columns = "#BB5566",
                       pch.rows = 16, pch.columns = 17,
                       cex.rows = graphics::par("cex"),
                       cex.columns = graphics::par("cex"),
                       xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                       xlab = NULL, ylab = NULL,
                       axes = TRUE, frame.plot = axes,
                       ann = graphics::par("ann"),
                       panel.first = NULL, panel.last = NULL) {

  ## Save and restore graphical parameters
  ## pty: square plotting region, independent of device size
  old_par <- graphics::par(pty = "s", no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- xlim %||% range(coord_row$x, coord_col$x, na.rm = TRUE, finite = TRUE)
  ylim <- ylim %||% range(coord_row$y, coord_col$y, na.rm = TRUE, finite = TRUE)
  graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)

  ## Evaluate pre-plot expressions
  panel.first

  ## Plot
  graphics::abline(h = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  graphics::abline(v = 0, lty = "dashed", lwd = 1, col = graphics::par("fg"))
  if (rows) {
    graphics::points(x = coord_row$x, y = coord_row$y, col = col.rows,
                     pch = pch.rows, cex = cex.rows)
  }
  if (columns) {
    graphics::points(x = coord_col$x, y = coord_col$y, col = col.columns,
                     pch = pch.columns, cex = cex.columns)
  }

  ## Labels
  if (!is.null(labels)) {
    labels <- match.arg(labels, several.ok = TRUE)
    if (any(labels == "rows") | any(labels == "individuals")) {
      viz_labels(x = coord_row$x, y = coord_row$y, labels = coord_row$label,
                 col = col.rows, ...)
    }
    if (any(labels == "columns") | any(labels == "variables")) {
      viz_labels(x = coord_col$x, y = coord_col$y, labels = coord_col$label,
                 col = col.columns, ...)
    }
  }

  ## Evaluate post-plot and pre-axis expressions
  panel.last

  ## Construct axis
  if (axes) {
    graphics::axis(side = 1, las = 1)
    graphics::axis(side = 2, las = 1)
  }

  ## Plot frame
  if (frame.plot) {
    graphics::box()
  }

  ## Add annotation
  if (ann) {
    graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab)
  }
}
