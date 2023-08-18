# GET DATA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_data
#' @aliases get_data,MultivariateAnalysis-method
setMethod(
  f = "get_data",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) {
    as.data.frame(x@data)
  }
)
