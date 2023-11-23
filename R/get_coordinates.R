# GET COORDINATES
#' @include AllGenerics.R
NULL

# Coordinates ==================================================================
#' @export
#' @rdname get_coordinates
#' @aliases get_coordinates,MultivariateAnalysis-method
setMethod(
  f = "get_coordinates",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, principal = TRUE, sup_name = ".sup") {
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

    # P = sqrt(eigenvalue) X S
    if (!principal) {
      coords <- t(t(coords) / x@singular_values)
    }

    coords <- as.data.frame(coords, row.names = id)
    coords[[sup_name]] <- suppl

    coords
  }
)

# Replications =================================================================
#' @export
#' @rdname get_coordinates
#' @aliases get_replications,MultivariateBootstrap-method
setMethod(
  f = "get_replications",
  signature = c(x = "MultivariateBootstrap"),
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
  signature = c(x = "BootstrapPCA"),
  definition = function(x) {
    methods::callNextMethod(x = x, margin = 2)
  }
)
