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
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    # Remove non-numeric variables, if any
    quali <- !vapply(object, FUN = is.numeric, FUN.VALUE = logical(1))
    if (any(quali)) {
      old <- object
      object <- object[, -c(which(quali), sup_col), drop = FALSE]
      if (!is.null(sup_col)) {
        object <- cbind(object, old[, sup_col, drop = FALSE])
        sup_col <- utils::tail(seq_along(object), length(sup_col))
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
                         rank = rank, sup_row = sup_row, sup_col = sup_col,
                         weight_row = weight_row, weight_col = weight_col)
  }
)

#' @export
#' @rdname pca
#' @aliases pca,matrix-method
setMethod(
  f = "pca",
  signature = signature(object = "matrix"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    # Check missing values
    if (anyNA(object))
      stop("Missing values detected.", call. = FALSE)

    # Fix dimension names
    names_row <- rownames(object)
    names_col <- colnames(object)
    if (is.null(names_row)) names_row <- as.character(seq_len(nrow(object)))
    if (is.null(names_col)) names_col <- as.character(seq_len(ncol(object)))

    # Subset
    is_row_sup <- is_supplementary(sup_row, nrow(object))
    is_col_sup <- is_supplementary(sup_col, ncol(object))
    N <- object[!is_row_sup, !is_col_sup, drop = FALSE]

    # Dimension of the solution
    ndim <- min(rank, dim(N) - 1)
    dim_keep <- seq_len(ndim)
    i <- nrow(N)
    j <- ncol(N)

    # Weights
    w_row <- if (is.null(weight_row)) rep(1, nrow(N)) else weight_row
    w_col <- if (is.null(weight_col)) rep(1, ncol(N)) else weight_col
    w_row <- w_row / sum(w_row)
    W_row <- sqrt(w_row)
    W_col <- sqrt(w_col)

    # Build matrix
    # matrix * vector is faster (!) than:
    # matrix %*% t(vector)
    # t(t(matrix) * vector)
    s_row <- sqrt(w_row)
    s_col <- sqrt(w_col)
    W_row1 <- matrix(s_row, nrow = i, ncol = j, byrow = FALSE)
    W_col1 <- matrix(s_col, nrow = i, ncol = j, byrow = TRUE)
    W_row2 <- matrix(s_row, nrow = i, ncol = ndim, byrow = FALSE)
    W_col2 <- matrix(s_col, nrow = j, ncol = ndim, byrow = FALSE)

    # Center data
    if (center) {
      var_mean <- weighted_mean(N, w_row)
    } else {
      var_mean <- rep(0, j)
    }
    ctr <- matrix(var_mean, nrow = i, ncol = j, byrow = TRUE)
    P <- N - ctr

    # Scale data
    if (scale) {
      var_sd <- weighted_sd(P, w_row)
    } else {
      var_sd <- rep(1, j)
    }
    std <- matrix(var_sd, nrow = i, ncol = j, byrow = TRUE)
    M <- P / std

    # Matrix of standardized residuals
    S <- M * W_col1 * W_row1

    # Singular Value Decomposition
    D <- svd(S)
    sv <- D$d[dim_keep] # Singular values

    # Standard coordinates
    U <- D$u[, dim_keep, drop = FALSE] / W_row2
    V <- D$v[, dim_keep, drop = FALSE] / W_col2

    sv_U <- matrix(sv, nrow = i, ncol = ndim, byrow = TRUE)
    sv_V <- matrix(sv, nrow = j, ncol = ndim, byrow = TRUE)

    # Principal coordinates
    coord_row <- U * sv_U
    coord_col <- V * sv_V

    # Contributions
    contrib_row <- ((coord_row * W_row2) / sv_U)^2 * 100
    contrib_col <- ((coord_col * W_col2) / sv_V)^2 * 100

    # Squared distance to centroide
    dist_row <- rowSums((M * W_col1)^2)
    dist_col <- colSums((M * W_row1)^2)

    # Supplementary points
    if (any(is_row_sup)) {
      extra_row <- object[is_row_sup, !is_col_sup, drop = FALSE]
      ind_sup <- (t(extra_row) - var_mean) * w_col / var_sd

      # Coordinates
      coord_row_sup <- crossprod(ind_sup, V)
      coord_row <- rbind(coord_row, coord_row_sup)

      # Distances
      dist_row_sup <- colSums(ind_sup^2 * w_col)
      dist_row <- c(dist_row, dist_row_sup)
    }
    if (any(is_col_sup)) {
      extra_col <- object[!is_row_sup, is_col_sup, drop = FALSE]
      # Center and scale
      if (center) {
        extra_col <- t(t(extra_col) - weighted_mean(extra_col, w_row))
      }
      if (scale) {
        extra_col <- t(t(extra_col) / weighted_sd(extra_col, w_row))
      }
      var_sup <- extra_col * w_row

      # Coordinates
      coord_col_sup <- crossprod(var_sup, U)
      coord_col <- rbind(coord_col, coord_col_sup)

      # Distances
      dist_col_sup <- colSums(extra_col^2 * w_row)
      dist_col <- c(dist_col, dist_col_sup)
    }

    # Squared cosine
    cos_row <- coord_row^2 / dist_row
    cos_col <- coord_col^2 / dist_col

    names(sv) <- paste0("F", dim_keep)
    .PCA(
      data = object,
      dimension = as.integer(ndim),
      singular_values = sv,
      rows = build_results(
        names = names_row,
        principal = coord_row,
        standard = U,
        contributions = contrib_row,
        distances = sqrt(dist_row),
        cosine = cos_row,
        weights = w_row,
        supplement = is_row_sup
      ),
      columns = build_results(
        names = names_col,
        principal = coord_col,
        standard = V,
        contributions = contrib_col,
        distances = sqrt(dist_col),
        cosine = cos_col,
        weights = w_col,
        supplement = is_col_sup
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
    colnames(coords) <- paste0("F", seq_along(coords))
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
