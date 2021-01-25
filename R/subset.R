# ACCESSORS
#' @include AllClasses.R
NULL

# Coerce =======================================================================
#' @method as.list MultivariateResults
as.list.MultivariateResults <- function(x, ...) {
  list(
    # names = x@names,
    coordinates = x@coordinates,
    # standard = x@standard,
    contributions = x@contributions,
    cos2 = x@cosine,
    # distances = x@distances,
    # weights = x@weights,
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

#' @export
#' @rdname subset
#' @aliases [[,CA,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "CA", i = "ANY", j = "missing"),
  definition = function(x, i) {
    data <- as.list(x)
    methods::callGeneric(x = data, i = i)
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PCA,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PCA", i = "ANY", j = "missing"),
  definition = function(x, i) {
    data <- as.list(x)
    data[[1]] <- list(
      data = x@data,
      mean = x@center,
      sd = x@scale
    )
    data[[3]][["cor"]] <- sqrt(x@columns@cosine)
    names(data) <- c("data", "individuals", "variables", "eigenvalues")
    methods::callGeneric(x = data, i = i)
  }
)
