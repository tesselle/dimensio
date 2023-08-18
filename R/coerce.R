# COERCE
#' @include AllGenerics.R
NULL

#' @method as.list MultivariateResults
as.list.MultivariateResults <- function(x, ...) {
  list(
    # names = x@names,
    coordinates = x@principal,
    # standard = x@standard,
    contributions = x@contributions,
    cos2 = x@cosine,
    # distances = x@distances,
    masses = x@weights,
    supplement = x@supplement
  )
}

#' @method as.list MultivariateAnalysis
#' @export
as.list.MultivariateAnalysis <- function(x, ...) {
  list(
    data = x@data,
    rows = as.list(x@rows),
    columns = as.list(x@columns),
    eigenvalues = x@singular_values^2
  )
}
