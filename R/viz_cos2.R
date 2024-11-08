# PLOT COS2
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_contributions
#' @aliases viz_cos2,MultivariateAnalysis-method
setMethod(
  f = "viz_cos2",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 2, axes = 1, active = TRUE,
                        sup = TRUE, sort = TRUE, decreasing = TRUE,
                        limit = 10, horiz = FALSE,
                        col = "grey90", border = "grey10") {
    ## Prepare data
    data <- prepare_cos2(x, margin = margin, axes = axes,
                         active = active, sup = sup, sort = sort,
                         decreasing = decreasing, limit = limit)

    xx <- paste0("(F", axes, ")", collapse = "-")
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

  ## Prevent reordering
  data$x <- factor(data$x, levels = unique(data$x))

  data
}
