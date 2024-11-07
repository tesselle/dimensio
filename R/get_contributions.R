# GET CONTRIBUTIONS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_contributions
#' @aliases get_contributions,MultivariateAnalysis-method
setMethod(
  f = "get_contributions",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {
    arkhe::assert_scalar(margin, "numeric")

    if (margin == 1) contrib <- x@rows@contributions
    if (margin == 2) contrib <- x@columns@contributions

    as.data.frame(contrib)
  }
)
