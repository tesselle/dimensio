# MUTATORS
#' @include AllClasses.R
NULL

# Non exported =================================================================
get_masses <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) mass <- x@rows@weights
  if (margin == 2) mass <- x@columns@weights
  mass
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
get_groups <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) grp <- x@rows@groups
  if (margin == 2) grp <- x@columns@groups
  grp
}
get_order <- function(x, margin = 1) {
  margin <- margin[[1L]]
  if (margin == 1) ord <- x@rows@order
  if (margin == 2) ord <- x@columns@order
  ord
}
is_centered <- function(x) {
  !all(x@center == 0)
}
is_scaled <- function(x) {
  !all(x@scale == 1)
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
  definition = function(x, margin = 1, principal = TRUE, sup_name = ".sup") {
    margin <- margin[[1L]]
    if (margin == 1) {
      coords <- if (principal) x@rows@principal else x@rows@standard
      suppl <- x@rows@supplement
      id <- x@rows@names
    }
    if (margin == 2) {
      coords <- if (principal) x@columns@principal else x@columns@standard
      suppl <- x@columns@supplement
      id <- x@columns@names
    }

    coords <- as.data.frame(coords, row.names = id)
    coords[[sup_name]] <- if (principal) suppl else suppl[!suppl]

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
    pvar <- eig / sum(eig) * 100 # Percentage
    cvar <- cumsum(pvar) # Cumulative percentage

    z <- data.frame(eig, pvar, cvar)
    expl <- if (methods::is(x, "CA")) "inertia" else "variance"
    colnames(z) <- c("eigenvalues", expl, "cumulative")
    z
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
