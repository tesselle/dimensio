# SUBSET
#' @include AllGenerics.R
NULL

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
    methods::callGeneric(x = data, i = i)
  }
)
