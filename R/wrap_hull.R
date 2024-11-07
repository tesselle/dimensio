# CONVEX HULL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname wrap
#' @aliases wrap_hull,MultivariateAnalysis-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL) {
    ## Validation
    arkhe::assert_scalar(margin, "numeric")
    arkhe::assert_type(axes, "numeric")
    arkhe::assert_length(axes, 2)

    ## Get coordinates
    data <- get_coordinates(x, margin = margin)
    data <- data[, axes]
    n <- nrow(data)

    ## Add groups, if any
    if (length(group) > 1) {
      arkhe::assert_length(group, n)
      group <- group[get_order(x, margin = margin)]
    } else if (length(group) == 1) {
      group <- get_extra(x)[[group]]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    } else {
      group <- rep("", n)
    }
    group <- as.character(group)

    data <- split(data, f = group)
    lapply(
      X = data,
      FUN = function(x) {
        ## Drop NAs
        x <- stats::na.omit(x)
        if (nrow(x) == 0) return(NULL)

        i <- grDevices::chull(x[, c(1, 2)])
        x[c(i, i[1]), , drop = FALSE]
      }
    )
  }
)
