# ENVELOPES
#' @include AllClasses.R
NULL

#' @export
#' @rdname envelopes
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
    if (!is.null(group)) {
      assert_length(group, nrow(data))
      group <- group[k]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    } else {
      group <- rep("conv. hull", length(k))
    }
    data$group <- group

    data <- split(data, f = group)
    hull <- lapply(
      X = data,
      FUN = function(x) {
        i <- grDevices::chull(x[, c(1, 2)])
        x[c(i, i[1]), , drop = FALSE]
      }
    )
    hull <- do.call(rbind, hull)
    as.data.frame(hull)
  }
)

#' @export
#' @rdname envelopes
#' @aliases wrap_hull,MultivariateBootstrap-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "MultivariateBootstrap"),
  definition = function(x, margin = 1, axes = c(1, 2)) {
    ## Get groups
    group  <- get_groups(x, margin = margin)
    methods::callNextMethod(x = x, margin = margin, axes = axes, group = group)
  }
)

#' @export
#' @rdname envelopes
#' @aliases wrap_hull,BootstrapPCA-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "BootstrapPCA"),
  definition = function(x, axes = c(1, 2)) {
    methods::callNextMethod(x = x, margin = 2, axes = axes)
  }
)
