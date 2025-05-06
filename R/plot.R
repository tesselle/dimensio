# PLOT COORDINATES
#' @include AllGenerics.R
NULL

#' @export
#' @method plot PCOA
plot.PCOA <- function(x, ..., axes = c(1, 2), labels = FALSE,
                      extra_quali = NULL, extra_quanti = NULL,
                      ellipse = NULL, hull = FALSE,
                      color = NULL, fill = FALSE, symbol = FALSE, size = c(1, 6),
                      xlim = NULL, ylim = NULL, main = NULL, sub = NULL,
                      ann = graphics::par("ann"), frame.plot = TRUE,
                      panel.first = NULL, panel.last = NULL,
                      legend = list(x = "topleft")) {
  ## Set axes
  arkhe::assert_type(axes, "numeric")
  arkhe::assert_length(axes, 2)
  assign("axes", value = axes, envir = the)

  ## Prepare data
  coord <- get_coordinates(x)
  coord$x <- coord[[axes[[1L]]]]
  coord$y <- coord[[axes[[2L]]]]
  n <- NROW(coord)

  ## Recycle graphical parameters if of length one
  dots <- list(...)
  col <- recycle(dots$col %||% graphics::par("col"), n)
  bg <- recycle(dots$bg %||% graphics::par("bg"), n)
  pch <- recycle(dots$pch %||% 16, n)
  cex <- recycle(dots$cex %||% graphics::par("cex"), n)

  ## Highlight quantitative information
  if (length(extra_quanti) > 0) {
    arkhe::assert_type(extra_quanti, "numeric")
    arkhe::assert_length(extra_quanti, n)
    if (!isFALSE(color)) col <- khroma::palette_color_continuous(colors = color)(extra_quanti)
    if (!isFALSE(fill)) bg <- khroma::palette_color_continuous(colors = fill)(extra_quanti)
    if (!isFALSE(size)) cex <- khroma::palette_size_sequential(range = size)(extra_quanti)
  }
  ## Highlight qualitative information
  if (length(extra_quali) > 0) {
    arkhe::assert_length(extra_quali, n)
    if (!isFALSE(color)) col <- khroma::palette_color_discrete(colors = color)(extra_quali)
    if (!isFALSE(fill)) bg <- khroma::palette_color_discrete(colors = fill)(extra_quali)
    if (!isFALSE(symbol)) pch <- khroma::palette_shape(symbols = symbol)(extra_quali)
  }

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
  graphics::points(x = coord$x, y = coord$y,
                   col = col, bg = bg, pch = pch, cex = cex)

  ## Labels
  if (isTRUE(labels)) {
    label(
      x = coord$x,
      y = coord$y,
      labels = rownames(coord),
      type = "shadow",
      col = col,
      cex = cex,
      xpd = TRUE
    )
  }

  if (length(extra_quali) > 0) {
    ## Add ellipse
    if (is.list(ellipse) && length(ellipse) > 0) {
      args_ell <- list(x = x, group = extra_quali, axes = axes,
                       color = color, fill = FALSE, symbol = FALSE)
      ellipse <- modifyList(args_ell, val = ellipse)
      do.call(viz_ellipses, ellipse)
    }
    ## Add convex hull
    if (isTRUE(hull)) {
      args_hull <- list(x = x, group = extra_quali, axes = axes,
                        color = color, fill = FALSE, symbol = FALSE)
      do.call(viz_hull, args_hull)
    }
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
      xlab = colnames(coord)[axes[[1]]],
      ylab = colnames(coord)[axes[[2]]]
    )
  }

  ## Legend
  coord <- data.frame(
    extra_quanti = if (length(extra_quanti) > 0) extra_quanti else rep(NA, n),
    extra_quali = if (length(extra_quali) > 0) extra_quali else rep(NA, n),
    cex = cex, col = col, bg = bg, pch = pch, lty = rep(NA, n)
  )
  prepare_legend(coord, legend, points = TRUE, lines = FALSE)

  invisible(x)
}

#' @export
#' @rdname plot
#' @aliases plot,PCOA,missing-method
setMethod("plot", c(x = "PCOA", y = "missing"), plot.PCOA)
