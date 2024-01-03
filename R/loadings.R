# LOADINGS
#' @include AllGenerics.R
NULL

# PCA ==========================================================================
#' @export
#' @method loadings PCA
loadings.PCA <- function(x, ...) {
  loads <- x@columns@standard
  class(loads) <- "loadings"
  return(loads)
}

#' @export
#' @rdname loadings
#' @aliases loadings,PCA-method
setMethod("loadings", c(x = "PCA"), loadings.PCA)
