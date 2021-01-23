# ACCESSORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname subset
#' @aliases [[,PCA,character,missing-method
setMethod(
  f = "[[",
  signature = c(x = "CA", i = "character", j = "missing"),
  definition = function(x, i) {
    choice <- match.arg(
      arg = i,
      choices = c("data", "rows", "columns", "eigenvalues"),
      several.ok = FALSE
    )

    switch (
      choice,
      data = list(
        x@data
      ),
      rows = list(
        coord = x@row_coordinates,
        cos2 = x@row_cosine,
        masses = x@row_weights,
        sup = x@row_supplement
      ),
      columns = list(
        coord = x@column_coordinates,
        cos2 = x@column_cosine,
        masses = x@column_weights,
        sup = x@column_supplement
      ),
      eigenvalues = x@singular_values^2
    )
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PCA,character,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PCA", i = "character", j = "missing"),
  definition = function(x, i) {
    choice <- match.arg(
      arg = i,
      choices = c("data", "individuals", "variables", "eigenvalues"),
      several.ok = FALSE
    )

    switch (
      choice,
      data = list(
        data = x@data,
        mean = x@center,
        sd = x@standard_deviation
      ),
      individuals = list(
        coord = x@row_coordinates,
        cos2 = x@row_cosine,
        weights = x@row_weights,
        sup = x@row_supplement
      ),
      variables = list(
        coord = x@column_coordinates,
        cor = sqrt(x@column_cosine),
        cos2 = x@column_cosine,
        weights = x@column_weights,
        sup = x@column_supplement
      ),
      eigenvalues = x@singular_values^2
    )
  }
)
