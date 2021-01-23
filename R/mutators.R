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

# Contributions ================================================================
#' @export
#' @rdname mutator
#' @aliases get_contributions,MultivariateAnalysis-method
setMethod(
  f = "get_contributions",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {
    coords <- get_coordinates(x, margin = margin, sup = FALSE)

    sv <- x@singular_values
    if (margin == 1) weights <- x@row_weights
    if (margin == 2) weights <- x@column_weights

    coords <- as.matrix(coords)
    contrib <- t(t(coords^2 * weights) / sv^2) * 100
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

    if (margin == 1) {
      coords <- x@row_coordinates
      suppl <- x@row_supplement
      name <- x@row_names
    }
    if (margin == 2) {
      coords <- x@column_coordinates
      suppl <- x@column_supplement
      name <- x@column_names
    }

    coords <- as.data.frame(coords)
    rownames(coords) <- name

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
    if (margin == 1) {
      cos2 <- x@row_cosine
      suppl <- x@row_supplement
      name <- x@row_names
    }
    if (margin == 2) {
      cos2 <- x@column_cosine
      suppl <- x@column_supplement
      name <- x@column_names
    }

    cos2 <- as.data.frame(cos2)
    rownames(cos2) <- name

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
    if (margin == 1) {
      d2 <- x@row_distances
      names(d2) <- x@row_names
    }
    if (margin == 2) {
      d2 <- x@column_distances
      names(d2) <- x@column_names
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
      masses <- x@row_weights
      d2 <- x@row_distances
      suppl <- x@row_supplement
      name <- x@row_names
    }
    if (margin == 2) {
      masses <- x@column_weights
      d2 <- x@column_distances
      suppl <- x@column_supplement
      name <- x@column_names
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
