# GET EIGENVALUES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname get_eigenvalues
#' @aliases get_eigenvalues,MultivariateAnalysis-method
setMethod(
  f = "get_eigenvalues",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x) {
    eig <- x@singular_values^2 # Eigenvalues
    pvar <- eig / sum(eig) * 100 # Percentage
    cvar <- cumsum(pvar) # Cumulative percentage

    z <- data.frame(eig, pvar, cvar)
    colnames(z) <- c("eigenvalues", "variance", "cumulative")
    z
  }
)
