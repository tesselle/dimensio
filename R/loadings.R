# LOADINGS
#' @include AllGenerics.R
NULL

# PCA ==========================================================================
#' @export
#' @rdname loadings
#' @aliases loadings,PCA-method
setMethod(
  f = "loadings",
  signature = c(x = "PCA"),
  definition = function(x) {
    loads <- x@columns@standard
    class(loads) <- "loadings"
    return(loads)
  }
)
