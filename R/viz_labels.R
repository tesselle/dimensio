# PLOT LABELS
#' @include AllGenerics.R
NULL

#' Non-Overlapping Text Labels
#'
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param top An [`integer`] specifying the number of labels to draw.
#'  Only the labels of the `top` \eqn{n} observations will be drawn.
#'  If `NULL`, all labels are drawn.
#' @param box A [`logical`] scalar: should a box be drawn underneath labels?
#' @param segment A [`logical`] scalar: should segments be drawn?
#' @param ... Further [graphical parameters][graphics::par], particularly,
#'  character expansion, `cex` and color, `col`.
#' @inheritParams prepare
#' @author N. Frerebeau
#' @docType methods
# @family plot methods
#' @aliases viz_labels-method
#' @keywords internal
#' @export
setGeneric(
  name = "viz_labels",
  def = function(x, ...) standardGeneric("viz_labels")
)

#' @rdname viz_labels
#' @aliases viz_labels,MultivariateAnalysis-method
setMethod(
  f = "viz_labels",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin, ..., axes = c(1, 2),
                        active = TRUE, sup = TRUE,
                        highlight = NULL, top = 10,
                        box = FALSE, segment = FALSE) {
    ## Prepare data
    coord <- prepare(x, margin = margin, axes = axes, active = active,
                     sup = sup, highlight = highlight, ...)

    ## Select
    if (!is.null(top) && top > 0) {
      top <- min(nrow(coord), top)
      if (is.numeric(coord$z)) {
        ## Get order
        k <- order(coord$z, decreasing = TRUE)[seq_len(top)]
        ## Subset
        coord <- coord[k, , drop = FALSE]
      }
    }

    ## Clean
    coord <- coord[!is.na(coord$cex), , drop = FALSE]

    arkhe::label_auto(
      x = coord$x, y = coord$y,
      labels = coord$label,
      segment = segment, box = box,
      cex = coord$cex,
      col = coord$col
    )
  }
)
