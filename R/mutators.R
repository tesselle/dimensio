# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname mutator
#' @aliases dim,CA-method
setMethod(
  f = "dim",
  signature = signature(x = "CA"),
  definition = function(x) x@dimension
)

#' @export
#' @rdname mutator
#' @aliases get_eigenvalues,CA-method
setMethod(
  f = "get_eigenvalues",
  signature = signature(x = "CA"),
  definition = function(x) {
    eig <- x@singular_values^2 # Eigenvalues
    pvar <- eig / sum(eig) * 100 # Percentage of variance
    cvar <- cumsum(pvar) # Cumulative percentage of variance

    data.frame(
      eigenvalues = eig,
      variance = pvar,
      cumulative = cvar,
      row.names =  paste0("CA", seq_along(eig))
    )
  }
)

#' @export
#' @rdname mutator
#' @aliases get_inertia,CA-method
setMethod(
  f = "get_inertia",
  signature = signature(x = "CA"),
  definition = function(x, margin = 1) {
    if (margin == 1) {
      i <- x@row_inertias
      names(i) <- x@row_names
    }
    if (margin == 2) {
      i <- x@column_inertias
      names(i) <- x@column_names
    }
    i
  }
)

#' @export
#' @rdname mutator
#' @aliases get_distances,CA-method
setMethod(
  f = "get_distances",
  signature = signature(x = "CA"),
  definition = function(x, margin = 1) {
    if (margin == 1) {
      d <- x@row_distances
      names(d) <- x@row_names
    }
    if (margin == 2) {
      d <- x@column_distances
      names(d) <- x@column_names
    }
    d
  }
)

#' @export
#' @rdname mutator
#' @aliases get_variance,CA-method
setMethod(
  f = "get_variance",
  signature = signature(x = "CA"),
  definition = function(x, digits = 2) {
    eig <- x@singular_values^2
    pc <- round(eig / sum(eig) * 100, digits = digits)
    colnames(pc) <- paste0("CA", seq_along(pc))
    return(pc)
  }
)
