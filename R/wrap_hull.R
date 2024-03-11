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
    ## Get coordinates
    data <- get_coordinates(x, margin = margin)
    data <- data[, axes]

    ## Add groups, if any
    k <- get_order(x, margin = margin)
    if (length(group) == 1) group <- get_extra(x)[[group]]
    if (length(group) > 0) {
      arkhe::assert_length(group, nrow(data))
      group <- group[k]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    } else {
      group <- rep("", length(k))
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
