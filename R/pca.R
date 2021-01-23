# PRINCIPAL COMPONENTS ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname pca
#' @aliases pca,data.frame-method
setMethod(
  f = "pca",
  signature = signature(object = "data.frame"),
  definition = function(object, scale = TRUE, n = NULL,
                        sup_ind = NULL, sup_var = NULL) {
    object <- as.matrix(object)
    pca(object = object, scale = scale, n = n,
        sup_ind = sup_ind, sup_var = sup_var)
  }
)

#' @export
#' @rdname pca
#' @aliases pca,matrix-method
setMethod(
  f = "pca",
  signature = signature(object = "matrix"),
  definition = function(object, scale = TRUE, n = NULL,
                        sup_ind = NULL, sup_var = NULL,
                        weight_ind = NULL, weight_var = NULL) {
    # Check missing values
    if (anyNA(object))
      stop("Missing values detected.", call. = FALSE)

    # Fix dimension names
    names_ind <- rownames(object)
    names_var <- colnames(object)
    if (is.null(names_ind)) names_ind <- as.character(seq_len(nrow(object)))
    if (is.null(names_var)) names_var <- as.character(seq_len(ncol(object)))

    # Subset
    is_ind_sup <- is_supplementary(sup_ind, nrow(object))
    is_var_sup <- is_supplementary(sup_var, ncol(object))
    N <- object[!is_ind_sup, !is_var_sup, drop = FALSE]

    # Dimension of the solution
    ndim <- min(n, ncol(N) - 1)
    keep_dim <- seq_len(ndim)

    # Weights
    w_ind <- if (is.null(weight_ind)) rep(1, nrow(N)) else weight_ind
    w_var <- if (is.null(weight_var)) rep(1, ncol(N)) else weight_var
    w_ind <- w_ind / sum(w_ind)
    W_ind <- sqrt(w_ind)
    W_var <- sqrt(w_var)

    # Center data
    center <- weighted_mean(N, w_ind)
    P <- t(t(N) - center)

    # Scale data
    if (scale) {
      std_dev <- weighted_sd(P, w_ind)
    } else {
      std_dev <- rep(1, length(center))
    }
    M <- t(t(P) / std_dev)

    # Matrix of standardized residuals
    S <- t(t(M) * W_var * W_ind)

    # Singular Value Decomposition
    D <- svd(S)
    sv <- D$d[keep_dim] # Singular values

    # Standard coordinates
    U <- D$u[, keep_dim, drop = FALSE] / W_ind
    V <- D$v[, keep_dim, drop = FALSE] / W_var

    # Set names
    colnames(U) <- colnames(V) <- names(sv) <- paste0("PC", keep_dim)

    # Principal coordinates
    coord_ind <- t(t(U) * sv)
    coord_var <- t(t(V) * sv)

    # Squared distance to centroide
    dist_ind <- colSums(t(M^2) * w_var)
    dist_var <- colSums(M^2 * w_ind)

    # Supplementary points
    if (any(is_ind_sup)) {
      extra_ind <- object[is_ind_sup, !is_var_sup, drop = FALSE]
      ind_sup <- (t(extra_ind) - center) * w_var / std_dev

      # Coordinates
      coord_ind_sup <- crossprod(ind_sup, V)
      coord_ind <- rbind(coord_ind, coord_ind_sup)

      # Distances
      dist_ind_sup <- colSums(ind_sup^2 * w_var)
      dist_ind <- c(dist_ind, dist_ind_sup)
    }
    if (any(is_var_sup)) {
      extra_var <- object[!is_ind_sup, is_var_sup, drop = FALSE]
      # Center and scale
      extra_var <- t(t(extra_var) - weighted_mean(extra_var, w_ind))
      if (scale) {
        extra_var <- t(t(extra_var) / weighted_sd(extra_var, w_ind))
      }
      var_sup <- extra_var * w_ind

      # Coordinates
      coord_var_sup <- crossprod(var_sup, U)
      coord_var <- rbind(coord_var, coord_var_sup)

      # Distances
      dist_var_sup <- colSums(extra_var^2 * w_ind)
      dist_var <- c(dist_var, dist_var_sup)
    }

    # Squared cosine
    cos_ind <- coord_ind^2 / dist_ind
    cos_var <- coord_var^2 / dist_var

    .PCA(
      data = object,
      dimension = as.integer(ndim),
      row_names = names_ind,
      row_coordinates = coord_ind,
      row_standard = U,
      row_distances = sqrt(dist_ind),
      row_cosine = cos_ind,
      row_weights = w_ind,
      row_supplement = is_ind_sup,
      column_names = names_var,
      column_coordinates = coord_var,
      column_standard = V,
      column_distances = sqrt(dist_var),
      column_cosine = cos_var,
      column_weights = w_var,
      column_supplement = is_var_sup,
      singular_values = sv,
      center = center,
      standard_deviation = std_dev
    )
  }
)

#' @export
#' @rdname predict
#' @aliases predict,PCA-method
setMethod(
  f = "predict",
  signature = signature(object = "PCA"),
  definition = function(object, newdata, margin = 1) {
    # Coerce to matrix
    newdata <- if (missing(newdata)) object@data else as.matrix(newdata)

    # TODO: keep only matching rows/columns

    # Get standard coordinates
    if (margin == 1) {
      std <- object@column_standard
      weights <- object@column_weights
      center <- object@center
      std_dev <- object@standard_deviation

      newdata <- (t(newdata) - center) * weights / std_dev
    }
    if (margin == 2) {
      std <- object@row_standard
      weights <- object@row_weights
      center <- weighted_mean(newdata, weights)
      std_dev <- object@standard_deviation

      newdata <- t(t(newdata) - weighted_mean(newdata, weights))
      newdata <- t(t(newdata) / weighted_sd(newdata, weights))
      newdata <- newdata * weights
    }

    # Compute principal coordinates
    coords <- crossprod(newdata, std)
    coords <- as.data.frame(coords)
    colnames(coords) <- paste0("PC", seq_along(coords))
    return(coords)
  }
)

#' @export
#' @rdname mutator
#' @aliases get_correlations,PCA-method
setMethod(
  f = "get_correlations",
  signature = signature(x = "PCA"),
  definition = function(x, sup = TRUE, sup_name = ".sup") {
    corr <- sqrt(x@column_cosine)
    suppl <- x@column_supplement

    corr <- as.data.frame(corr)
    rownames(corr) <- x@column_names

    if (sup) {
      corr[[sup_name]] <- suppl
    } else {
      corr <- corr[!suppl, ]
    }

    return(corr)
  }
)

#' @export
#' @rdname mutator
#' @aliases loadings,PCA-method
setMethod(
  f = "loadings",
  signature = signature(x = "PCA"),
  definition = function(x) {
    loads <- x@column_coordinates / x@singular_values
    rownames(loads) <- x@column_names
    colnames(loads) <- paste0("PC", seq_len(ncol(loads)))
    class(loads) <- "loadings"
    return(loads)
  }
)
