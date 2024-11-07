# GET COS2
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_contributions
#' @aliases get_cos2,MultivariateAnalysis-method
setMethod(
  f = "get_cos2",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, sup_name = ".sup") {
    arkhe::assert_scalar(margin, "numeric")
    arkhe::assert_scalar(sup_name, "character")

    if (margin == 1) {
      cos2 <- x@rows@cosine
      suppl <- x@rows@supplement
    }
    if (margin == 2) {
      cos2 <- x@columns@cosine
      suppl <- x@columns@supplement
    }

    cos2 <- as.data.frame(cos2)
    cos2[[sup_name]] <- suppl

    cos2
  }
)
