# PLOT CONTRIBUTIONS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_contributions
#' @aliases viz_contributions,MultivariateAnalysis-method
setMethod(
  f = "viz_contributions",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., margin = 2, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        horiz = FALSE, col = "grey90", border = "grey10") {
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

  ## Prevent reordering
  data$x <- factor(data$x, levels = unique(data$x))

  data
}
