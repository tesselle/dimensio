# GET DATA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_data
#' @aliases get_data,MultivariateAnalysis-method
setMethod(
  f = "get_data",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x) {
    as.data.frame(x@data)
  }
)

# Supplementary variables ======================================================
is_supplementary <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) supp <- x@rows@supplement
  if (margin == 2) supp <- x@columns@supplement
  supp
}

has_supplementary <- function(x, margin = 1) {
  any(is_supplementary(x, margin = margin))
}

get_extra <- function(x) {
  as.data.frame(x@extra)
}

has_extra <- function(x) {
  all(lengths(x@extra) > 0)
}

`set_extra<-` <- function(x, value) {
  value <- lapply(
    X = value,
    FUN = function(val, i) { val[i] },
    i = get_order(x, margin = 1)
  )
  x@extra <- value
  methods::validObject(x)
  x
}
