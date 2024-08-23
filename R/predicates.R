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

has_supplementary <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) supp <- any(x@rows@supplement)
  if (margin == 2) supp <- any(x@columns@supplement)
  supp
}
