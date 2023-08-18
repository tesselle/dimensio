# GET VARIANCE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_eigenvalues
#' @aliases get_variance,MultivariateAnalysis-method
setMethod(
  f = "get_variance",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, digits = 2) {
    eig <- x@singular_values^2
    pc <- round(eig / sum(eig) * 100, digits = digits)
    return(pc)
  }
)
