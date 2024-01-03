# PLOT LABELS
#' @include AllGenerics.R
NULL

#' Non-Overlapping Text Labels
#'
#' @param x,y A [`numeric`] vector giving the x and y coordinates of a set of
#'  points. If `y` is missing, `x` must be a [`CA-class`], [`MCA-class`] or
#'  [`PCA-class`] object.
#' @param z A [`numeric`] vector giving an extra variable for subsetting.
#' @param labels A [`character`] vector specifying the text to be written.
#' @param margin A length-one [`numeric`] vector giving the subscript
#'  which the data will be returned: `1` indicates individuals/rows (the
#'  default), `2` indicates variables/columns.
#' @param axes A length-two [`numeric`] vector giving the dimensions to be
#'  plotted.
#' @param top An [`integer`] specifying the number of labels to draw.
#'  Only the labels of the `top` \eqn{n} observations along `z` will be drawn.
#'  If `NULL`, all labels are drawn.
#' @param box A [`logical`] scalar: should a box be drawn underneath labels?
#' @param segment A [`logical`] scalar: should segments be drawn?
#' @keywords internal
viz_labels <- function(x, y, z, labels, ..., margin = 1, axes = c(1, 2),
                       top = 10, box = FALSE, segment = FALSE,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"),
                       bg = graphics::par("bg"),
                       font = graphics::par("font")) {
  ## Get coordinates
  if (missing(y)) {
    if (missing(z)) {
      z <- joint_contributions(x, margin = margin, axes = axes)
    }
    coord <- prepare(x, margin = margin, axes = axes)
    x <- coord$x
    y <- coord$y
  }

  if (!is.null(top) && top > 0) {
    ## Reorder
    top <- min(length(x), top)
    k <- order(z, decreasing = TRUE)[seq_len(top)]

    ## Subset
    x <- x[k]
    y <- y[k]
    labels <- labels[k]
    if (length(cex) > 1) cex <- cex[k]
    if (length(col) > 1) col <- col[k]
    if (length(bg) > 1) bg <- bg[k]
    if (length(font) > 1) font <- font[k]
  }

  arkhe::label_auto(x = x, y = y, labels = labels, segment = segment, box = box,
                    cex = cex, col = col, bg = bg, font = font, ...)
}
