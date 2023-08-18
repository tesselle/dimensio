# GET INERTIA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_eigenvalues
#' @aliases get_inertia,MultivariateAnalysis-method
setMethod(
  f = "get_inertia",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {
    if (margin == 1) {
      masses <- x@rows@weights
      d2 <- x@rows@distances
      suppl <- x@rows@supplement
      name <- x@rows@names
    }
    if (margin == 2) {
      masses <- x@columns@weights
      d2 <- x@columns@distances
      suppl <- x@columns@supplement
      name <- x@columns@names
    }

    i <- masses * d2[!suppl]
    names(i) <- name[!suppl]
    i
  }
)
