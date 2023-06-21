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
  definition = function(x, axes = c(1, 2),
                        type = c("rows", "columns", "contributions"),
                        active = TRUE, sup = TRUE,
                        labels = c("rows", "columns"),
                        col.rows = "#004488", col.columns = "#BB5566",
                        pch.rows = 16, pch.columns = 17,
                        main = NULL, sub = NULL, ...) {
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
    cex.rows <- cex.columns <- graphics::par("cex")

    if (type == "contributions") {
      coord_row$x <- coord_row$x * sqrt(coord_row$mass)
      coord_row$y <- coord_row$y * sqrt(coord_row$mass)

      cex.rows <- cex.rows + (coord_row$mass / max(coord_row$mass))
      cex.columns <- cex.columns + (coord_col$mass / max(coord_col$mass))
    }

    .biplot(
      coord_row, coord_col, rows = TRUE, columns = TRUE, labels = labels,
      col.rows = col.rows, col.columns = col.columns,
      pch.rows = pch.rows, pch.columns = pch.columns,
      cex.rows = cex.rows,
      cex.columns = cex.columns,
      xlab = print_variance(x, axes[[1]]),
      ylab = print_variance(x, axes[[2]]),
      main = main, sub = sub,
      ...
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
  definition = function(x, axes = c(1, 2), type = c("form", "covariance"),
                        active = TRUE, sup = TRUE,
                        labels = c("individuals", "variables"),
                        col.rows = "#004488", col.columns = "#BB5566",
                        pch.rows = 16, pch.columns = 17,
                        lty = "solid", lwd = 1,
                        main = NULL, sub = NULL, ...) {
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
        length = 0.15, angle = 30,
        col = col.columns, lty = lty, lwd = lwd
      )
    }

    .biplot(
      coord_row, coord_col, rows = TRUE, columns = FALSE, labels = labels,
      col.rows = col.rows, col.columns = col.columns,
      pch.rows = pch.rows, pch.columns = pch.columns,
      xlab = print_variance(x, axes[[1]]),
      ylab = print_variance(x, axes[[2]]),
      main = main, sub = sub,
      panel.first = arrows_col(),
      panel.last = NULL,
      ...
    )

    invisible(x)
  }
)

.biplot <- function(coord_row, coord_col, rows = TRUE, columns = TRUE,
                    labels = c("rows", "columns", "individuals", "variables"),
                    col.rows = "#004488", col.columns = "#BB5566",
                    pch.rows = 16, pch.columns = 17,
                    cex.rows = graphics::par("cex"),
                    cex.columns = graphics::par("cex"),
                    xlab = NULL, ylab = NULL, main = NULL, sub = NULL,
                    axes = TRUE, frame.plot = axes,
                    ann = graphics::par("ann"),
                    panel.first = NULL, panel.last = NULL, ...) {

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(coord_row$x, coord_col$x)
  ylim <- range(coord_row$y, coord_col$y)
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
      lay_row <- wordcloud::wordlayout(
        x = coord_row$x,
        y = coord_row$y,
        words = coord_row$label,
        xlim = xlim,
        ylim = ylim,
        ...
      )
      graphics::text(x = lay_row[, "x"], y = lay_row[, "y"],
                     labels = coord_row$label, col = col.rows)
    }
    if (any(labels == "columns") | any(labels == "variables")) {
      lay_col <- wordcloud::wordlayout(
        x = coord_col$x,
        y = coord_col$y,
        words = coord_col$label,
        xlim = xlim,
        ylim = ylim,
        ...
      )
      graphics::text(x = lay_col[, "x"], y = lay_col[, "y"],
                     labels = coord_col$label, col = col.columns)
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
