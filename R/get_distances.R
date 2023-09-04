# GET DISTANCES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_distances
#' @aliases get_distances,MultivariateAnalysis-method
setMethod(
  f = "get_distances",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {

    margin <- margin[[1L]]
    if (margin == 1) {
      d2 <- x@rows@distances
      names(d2) <- x@rows@names
      suppl <- x@rows@supplement
    }
    if (margin == 2) {
      d2 <- x@columns@distances
      names(d2) <- x@columns@names
      suppl <- x@columns@supplement
    }

    d2
  }
)
