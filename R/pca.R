# PRINCIPAL COMPONENTS ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname pca
#' @aliases pca,data.frame-method
setMethod(
  f = "pca",
  signature = signature(object = "data.frame"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_ind = NULL, sup_var = NULL,
                        weight_ind = NULL, weight_var = NULL) {
    # Remove non-numeric variables, if any
    quali <- !vapply(object, FUN = is.numeric, FUN.VALUE = logical(1))
    if (any(quali)) {
      old <- object
      object <- object[, -c(which(quali), sup_var), drop = FALSE]
      if (!is.null(sup_var)) {
        object <- cbind(object, old[, sup_var, drop = FALSE])
        sup_var <- utils::tail(seq_along(object), length(sup_var))
      }
      # Generate message
      tot <- sum(quali)
      msg <- "%d qualitative %s removed: %s."
      txt <- ngettext(tot, "variable was", "variables were")
      col <- paste(colnames(old)[quali], collapse = ", ")
      message(sprintf(msg, tot, txt, col))
    }

    object <- as.matrix(object)
    methods::callGeneric(object = object, center = center, scale = scale,
                         rank = rank, sup_ind = sup_ind, sup_var = sup_var,
                         weight_ind = weight_ind, weight_var = weight_var)
  }
)

#' @export
#' @rdname pca
#' @aliases pca,matrix-method
setMethod(
  f = "pca",
  signature = signature(object = "matrix"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
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
    ndim <- min(rank, dim(N) - 1)
    dim_keep <- seq_len(ndim)
    i <- nrow(N)
    j <- ncol(N)

    # Weights
    w_ind <- if (is.null(weight_ind)) rep(1, nrow(N)) else weight_ind
    w_var <- if (is.null(weight_var)) rep(1, ncol(N)) else weight_var
    w_ind <- w_ind / sum(w_ind)
    W_ind <- sqrt(w_ind)
    W_var <- sqrt(w_var)

    # Build matrix
    # matrix * vector is faster (!) than:
    # matrix %*% t(vector)
    # t(t(matrix) * vector)
    s_ind <- sqrt(w_ind)
    s_var <- sqrt(w_var)
    W_ind1 <- matrix(s_ind, nrow = i, ncol = j, byrow = FALSE)
    W_var1 <- matrix(s_var, nrow = i, ncol = j, byrow = TRUE)
    W_ind2 <- matrix(s_ind, nrow = i, ncol = ndim, byrow = FALSE)
    W_var2 <- matrix(s_var, nrow = j, ncol = ndim, byrow = FALSE)

    # Center data
    if (center) {
      var_mean <- weighted_mean(N, w_ind)
    } else {
      var_mean <- rep(0, j)
    }
    ctr <- matrix(var_mean, nrow = i, ncol = j, byrow = TRUE)
    P <- N - ctr

    # Scale data
    if (scale) {
      var_sd <- weighted_sd(P, w_ind)
    } else {
      var_sd <- rep(1, j)
    }
    std <- matrix(var_sd, nrow = i, ncol = j, byrow = TRUE)
    M <- P / std

    # Matrix of standardized residuals
    S <- M * W_var1 * W_ind1

    # Singular Value Decomposition
    D <- svd(S)
    sv <- D$d[dim_keep] # Singular values

    # Standard coordinates
    U <- D$u[, dim_keep, drop = FALSE] / W_ind2
    V <- D$v[, dim_keep, drop = FALSE] / W_var2

    sv_U <- matrix(sv, nrow = i, ncol = ndim, byrow = TRUE)
    sv_V <- matrix(sv, nrow = j, ncol = ndim, byrow = TRUE)

    # Principal coordinates
    coord_ind <- U * sv_U
    coord_var <- V * sv_V

    # Contributions
    contrib_ind <- ((coord_ind * W_ind2) / sv_U)^2 * 100
    contrib_var <- ((coord_var * W_var2) / sv_V)^2 * 100

    # Squared distance to centroide
    dist_ind <- rowSums((M * W_var1)^2)
    dist_var <- colSums((M * W_ind1)^2)

    # Supplementary points
    if (any(is_ind_sup)) {
      extra_ind <- object[is_ind_sup, !is_var_sup, drop = FALSE]
      ind_sup <- (t(extra_ind) - var_mean) * w_var / var_sd

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
      if (center) {
        extra_var <- t(t(extra_var) - weighted_mean(extra_var, w_ind))
      }
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

    names(sv) <- paste0("PC", dim_keep)
    .PCA(
      data = object,
      dimension = as.integer(ndim),
      singular_values = sv,
      rows = .MultivariateResults(
        names = names_ind,
        principal = coord_ind,
        standard = U,
        contributions = contrib_ind,
        distances = sqrt(dist_ind),
        cosine = cos_ind,
        weights = w_ind,
        supplement = is_ind_sup,
        prefix = "PC"
      ),
      columns = .MultivariateResults(
        names = names_var,
        principal = coord_var,
        standard = V,
        contributions = contrib_var,
        distances = sqrt(dist_var),
        cosine = cos_var,
        weights = w_var,
        supplement = is_var_sup,
        prefix = "PC"
      ),
      center = var_mean,
      scale = var_sd
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
    if (missing(newdata)) {
      data <- object@data
      data <- data[!object@rows@supplement, !object@columns@supplement]
    } else {
      data <- as.matrix(newdata)
    }

    # Get standard coordinates
    var_mean <- object@center
    var_sd <- object@scale

    if (margin == 1) {
      std <- object@columns@standard
      w <- object@columns@weights

      newdata <- (t(newdata) - var_mean) * w / var_sd
    }
    if (margin == 2) {
      std <- object@rows@standard
      w <- object@rows@weights
      j <- ncol(newdata)

      X <- if (all(var_mean == 0)) rep(0, j) else weighted_mean(newdata, w)
      newdata <- t(t(newdata) - X)
      Y <- if (all(var_sd == 1)) rep(1, j) else weighted_sd(newdata, w)
      newdata <- t(t(newdata) / Y)
      newdata <- newdata * w
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
#' @aliases loadings,PCA-method
setMethod(
  f = "loadings",
  signature = signature(x = "PCA"),
  definition = function(x) {
    loads <- x@columns@standard
    class(loads) <- "loadings"
    return(loads)
  }
)
