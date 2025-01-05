# MUTATORS
#' @include AllGenerics.R
NULL

# Non exported =================================================================
is_centered <- function(x) {
  !all(x@center == 0)
}
is_scaled <- function(x) {
  !all(x@scale == 1)
}

get_masses <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) mass <- x@rows@weights
  if (margin == 2) mass <- x@columns@weights
  mass
}

get_order <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) ord <- x@rows@order
  if (margin == 2) ord <- x@columns@order
  ord
}

# Groups =======================================================================
get_groups <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) grp <- x@rows@groups
  if (margin == 2) grp <- x@columns@groups
  grp
}

`set_groups<-` <- function(x, margin = 1, value) {
  if (is.null(value)) value <- character(0)
  margin <- margin[[1L]]
  if (margin == 1) x@rows@groups <- value
  if (margin == 2) x@columns@groups <- value
  methods::validObject(x)
  x
}

has_groups <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) grp <- x@rows@groups
  if (margin == 2) grp <- x@columns@groups
  length(grp) > 0
}

# Dimensions ===================================================================
#' @export
#' @method dim MultivariateAnalysis
dim.MultivariateAnalysis <- function(x) {
  x@dimension
}

#' @export
#' @rdname dimnames
#' @aliases dim,MultivariateAnalysis-method
setMethod("dim", "MultivariateAnalysis", dim.MultivariateAnalysis)

#' @export
#' @method rownames MultivariateAnalysis
rownames.MultivariateAnalysis <- function(x, do.NULL = TRUE, prefix = "row") {
  dn <- dimnames(x)
  if (!is.null(dn[[1L]]))
    dn[[1L]]
  else {
    nr <- NROW(x@rows@principal)
    if (do.NULL)
      NULL
    else if (nr > 0L)
      paste0(prefix, seq_len(nr))
    else character()
  }
}

#' @export
#' @rdname dimnames
#' @aliases rownames,MultivariateAnalysis-method
setMethod("rownames", "MultivariateAnalysis", rownames.MultivariateAnalysis)

#' @export
#' @method colnames MultivariateAnalysis
colnames.MultivariateAnalysis <- function(x, do.NULL = TRUE, prefix = "col") {
  dn <- dimnames(x)
  if (!is.null(dn[[2L]]))
    dn[[2L]]
  else {
    nc <- NROW(x@columns@principal)
    if (do.NULL)
      NULL
    else if (nc > 0L)
      paste0(prefix, seq_len(nc))
    else character()
  }
}

#' @export
#' @rdname dimnames
#' @aliases colnames,MultivariateAnalysis-method
setMethod("colnames", "MultivariateAnalysis", colnames.MultivariateAnalysis)

#' @export
#' @method dimnames MultivariateAnalysis
dimnames.MultivariateAnalysis <- function(x) {
  list(x@rows@names, x@columns@names)
}

#' @export
#' @rdname dimnames
#' @aliases dimnames,MultivariateAnalysis-method
setMethod("dimnames", "MultivariateAnalysis", dimnames.MultivariateAnalysis)
