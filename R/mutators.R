# MUTATORS
#' @include AllClasses.R
NULL

# Dimensions ===================================================================
#' @export
#' @rdname mutator
#' @aliases dim,MultivariateAnalysis-method
setMethod(
  f = "dim",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) x@dimension
)

get_order <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) o <- x@rows@order
  if (margin == 2) o <- x@columns@order

  return(o)
}

# Contributions ================================================================
#' @export
#' @rdname mutator
#' @aliases get_contributions,MultivariateAnalysis-method
setMethod(
  f = "get_contributions",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {
    margin <- margin[[1L]]
    if (margin == 1) contrib <- x@rows@contributions
    if (margin == 2) contrib <- x@columns@contributions

    as.data.frame(contrib)
  }
)

# Coordinates ==================================================================
#' @export
#' @rdname mutator
#' @aliases get_coordinates,MultivariateAnalysis-method
setMethod(
  f = "get_coordinates",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, sup = TRUE, sup_name = ".sup") {

    margin <- margin[[1L]]
    if (margin == 1) {
      coords <- x@rows@coordinates
      suppl <- x@rows@supplement
    }
    if (margin == 2) {
      coords <- x@columns@coordinates
      suppl <- x@columns@supplement
    }

    coords <- as.data.frame(coords)

    if (sup) {
      coords[[sup_name]] <- suppl
    } else {
      coords <- coords[!suppl, ]
    }

    return(coords)
  }
)

# Cos2 =========================================================================
#' @export
#' @rdname mutator
#' @aliases get_cos2,MultivariateAnalysis-method
setMethod(
  f = "get_cos2",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, sup = TRUE, sup_name = ".sup") {

    margin <- margin[[1L]]
    if (margin == 1) {
      cos2 <- x@rows@cosine
      suppl <- x@rows@supplement
    }
    if (margin == 2) {
      cos2 <- x@columns@cosine
      suppl <- x@columns@supplement
    }

    cos2 <- as.data.frame(cos2)

    if (sup) {
      cos2[[sup_name]] <- suppl
    } else {
      cos2 <- cos2[!suppl, ]
    }

    return(cos2)
  }
)

# Data =========================================================================
#' @export
#' @rdname mutator
#' @aliases get_data,MultivariateAnalysis-method
setMethod(
  f = "get_data",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) {
    as.data.frame(x@data)
  }
)

# Distances ====================================================================
#' @export
#' @rdname mutator
#' @aliases get_distances,MultivariateAnalysis-method
setMethod(
  f = "get_distances",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {

    margin <- margin[[1L]]
    if (margin == 1) {
      d2 <- x@rows@distances
      names(d2) <- x@rows@names
    }
    if (margin == 2) {
      d2 <- x@columns@distances
      names(d2) <- x@columns@names
    }
    d2
  }
)

# Eigenvalues ==================================================================
#' @export
#' @rdname mutator
#' @aliases get_eigenvalues,MultivariateAnalysis-method
setMethod(
  f = "get_eigenvalues",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) {
    eig <- x@singular_values^2 # Eigenvalues
    pvar <- eig / sum(eig) * 100 # Percentage of variance
    cvar <- cumsum(pvar) # Cumulative percentage of variance

    data.frame(
      eigenvalues = eig,
      variance = pvar,
      cumulative = cvar
    )
  }
)

# Inertia ======================================================================
#' @export
#' @rdname mutator
#' @aliases get_inertia,MultivariateAnalysis-method
setMethod(
  f = "get_inertia",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {
    if (margin == 1) {
      masses <- x@rows@weights
      d2 <- x@rows@distances
      suppl <- x@rows@supplement
      name <- x@rows@names
    }
    if (margin == 2) {
      masses <- x@columns@weights
      d2 <- x@columns@distances
      suppl <- x@columns@supplement
      name <- x@columns@names
    }

    i <- masses * d2[!suppl]
    names(i) <- name[!suppl]
    i
  }
)

# Variance =====================================================================
#' @export
#' @rdname mutator
#' @aliases get_variance,MultivariateAnalysis-method
setMethod(
  f = "get_variance",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, digits = 2) {
    eig <- x@singular_values^2
    pc <- round(eig / sum(eig) * 100, digits = digits)
    return(pc)
  }
)
