# MUTATORS
#' @include AllClasses.R
NULL

# Non exported =================================================================
has_groups <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) grp <- length(x@rows@groups) > 0
  if (margin == 2) grp <- length(x@columns@groups) > 0
  grp
}
get_groups <- function(x, margin = 1) {
  grp1 <- grp2 <- NULL
  if (any(margin == 1)) grp1 <- x@rows@groups
  if (any(margin == 2)) grp2 <- x@columns@groups
  list(rows = grp1, columns = grp2)
}
get_order <- function(x, margin = 1) {
  ord1 <- ord2 <- NULL
  if (margin == 1) ord1 <- x@rows@order
  if (margin == 2) ord2 <- x@columns@order
  list(rows = ord1, columns = ord2)
}
is_centered <- function(x) {
  !all(x@center == 0)
}
is_scaled <- function(x) {
  !all(x@scale == 1)
}

# Dimensions ===================================================================
#' @export
#' @rdname mutators
#' @aliases dim,MultivariateAnalysis-method
setMethod(
  f = "dim",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) x@dimension
)

#' @export
#' @rdname mutators
#' @aliases rownames,MultivariateAnalysis-method
setMethod(
  f = "rownames",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) x@rows@names
)

#' @export
#' @rdname mutators
#' @aliases colnames,MultivariateAnalysis-method
setMethod(
  f = "colnames",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) x@columns@names
)

#' @export
#' @rdname mutators
#' @aliases dimnames,MultivariateAnalysis-method
setMethod(
  f = "dimnames",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x) list(x@rows@names, x@columns@names)
)

# Contributions ================================================================
#' @export
#' @rdname get_contributions
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
#' @rdname get_coordinates
#' @aliases get_coordinates,MultivariateAnalysis-method
setMethod(
  f = "get_coordinates",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, sup_name = ".sup") {
    margin <- margin[[1L]]
    if (margin == 1) {
      coords <- x@rows@principal
      suppl <- x@rows@supplement
      id <- x@rows@names
    }
    if (margin == 2) {
      coords <- x@columns@principal
      suppl <- x@columns@supplement
      id <- x@columns@names
    }

    coords <- as.data.frame(coords, row.names = id)
    coords[[sup_name]] <- suppl

    coords
  }
)

#' @export
#' @rdname get_coordinates
#' @aliases get_replications,MultivariateBootstrap-method
setMethod(
  f = "get_replications",
  signature = signature(x = "MultivariateBootstrap"),
  definition = function(x, margin = 1) {
    coords <- get_coordinates(x = x, margin = margin)

    k <- x@replications
    i <- nrow(coords) / (k + 1)
    j <- ncol(coords) - 1

    ## Drop the original data and the last column
    repl_coords <- coords[-seq_len(i), seq_len(j)]
    repl <- split(x = repl_coords, f = rep(seq_len(k), each = i))
    repl <- array(data = unlist(repl), dim = c(i, j, k))
    rownames(repl) <- rownames(coords)[seq_len(i)]
    colnames(repl) <- colnames(repl_coords)
    repl
  }
)

#' @export
#' @rdname get_coordinates
#' @aliases get_replications,BootstrapPCA-method
setMethod(
  f = "get_replications",
  signature = signature(x = "BootstrapPCA"),
  definition = function(x) {
    methods::callNextMethod(x = x, margin = 2)
  }
)

# Correlations =================================================================
#' @export
#' @rdname get_contributions
#' @aliases get_correlations,PCA-method
setMethod(
  f = "get_correlations",
  signature = signature(x = "PCA"),
  definition = function(x, sup_name = ".sup") {
    corr <- x@columns@principal / x@columns@distances
    suppl <- x@columns@supplement

    corr <- as.data.frame(corr)
    corr[[sup_name]] <- suppl

    corr
  }
)

# Cos2 =========================================================================
#' @export
#' @rdname get_contributions
#' @aliases get_cos2,MultivariateAnalysis-method
setMethod(
  f = "get_cos2",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, sup_name = ".sup") {

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
    cos2[[sup_name]] <- suppl

    cos2
  }
)

# Data =========================================================================
#' @export
#' @rdname get_data
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
#' @rdname get_distances
#' @aliases get_distances,MultivariateAnalysis-method
setMethod(
  f = "get_distances",
  signature = signature(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1) {

    margin <- margin[[1L]]
    if (margin == 1) {
      d2 <- x@rows@distances
      names(d2) <- x@rows@names
      suppl <- x@rows@supplement
    }
    if (margin == 2) {
      d2 <- x@columns@distances
      names(d2) <- x@columns@names
      suppl <- x@columns@supplement
    }

    d2
  }
)

# Eigenvalues ==================================================================
#' @export
#' @rdname get_eigenvalues
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
#' @rdname get_eigenvalues
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
#' @rdname get_eigenvalues
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
