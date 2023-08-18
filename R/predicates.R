# PREDICATES
#' @include AllGenerics.R
NULL

# Non exported =================================================================
is_centered <- function(x) {
  !all(x@center == 0)
}
is_scaled <- function(x) {
  !all(x@scale == 1)
}
#' @param index A [`numeric`] vector.
#' @param n An [`integer`] value.
#' @return A [`logical`] vector.
#' @keywords internal
#' @noRd
is_supplementary <- function(index, n) {
  x <- logical(n)

  if (is.null(index)) {
    return(x)
  } else {
    if (is.numeric(index)) {
      x[index] <- TRUE
      return(x)
    }
    if (is.logical(index)) {
      return(index)
    }
    arg <- deparse(substitute(index))
    msg <- sprintf("%s must be a numeric vector of indices.", sQuote(arg))
    stop(msg, call. = FALSE)
  }
}
has_supplementary <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) supp <- any(x@rows@supplement)
  if (margin == 2) supp <- any(x@columns@supplement)
  supp
}
has_groups <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) grp <- length(x@rows@groups) > 0
  if (margin == 2) grp <- length(x@columns@groups) > 0
  grp
}
