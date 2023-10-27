# PLOT LABELS
#' @include AllGenerics.R
NULL

viz_labels <- function(x, y, ..., labels = seq_along(x),
                       box = FALSE, segment = FALSE,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"),
                       bg = graphics::par("bg"),
                       font = graphics::par("font")) {

  arkhe::label_auto(x, y, labels = labels, segment = segment, box = box,
                    cex = cex, col = col, bg = bg, font = font, ...)
}
