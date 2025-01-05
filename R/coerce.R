# COERCE
#' @include AllGenerics.R
NULL

# To data.frame ================================================================
#' @export
#' @method as.data.frame MultivariateSummary
as.data.frame.MultivariateSummary <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x@results, row.names = row.names, optional = optional, ...)
}

#' @export
#' @rdname summary
#' @aliases as.data.frame,MultivariateSummary-method
setMethod("as.data.frame", "MultivariateSummary", as.data.frame.MultivariateSummary)

# To list ======================================================================
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
