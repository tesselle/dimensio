# GET CORRELATIONS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_contributions
#' @aliases get_correlations,PCA-method
setMethod(
  f = "get_correlations",
  signature = c(x = "PCA"),
  definition = function(x, sup_name = ".sup") {
    arkhe::assert_scalar(sup_name, "character")

    corr <- x@columns@principal / x@columns@distances
    suppl <- x@columns@supplement

    corr <- as.data.frame(corr)
    corr[[sup_name]] <- suppl

    corr
  }
)
