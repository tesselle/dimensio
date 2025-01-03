# PRINCIPAL COMPONENTS ANALYSIS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname pca
#' @aliases pca,data.frame-method
setMethod(
  f = "pca",
  signature = c(object = "data.frame"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_row = NULL, sup_col = NULL, sup_quali = NULL,
                        weight_row = NULL, weight_col = NULL, autoclean = TRUE) {
    ## Remove non-numeric variables, if any
    clean <- drop_variable(object, f = is.numeric, negate = TRUE,
                           sup = sup_col, extra = sup_quali, auto = autoclean,
                           what = "qualitative")

    ## Compute PCA
    results <- methods::callGeneric(
      object = clean$data, center = center, scale = scale,
      rank = rank, sup_row = sup_row, sup_col = clean$sup,
      weight_row = weight_row, weight_col = weight_col
    )

    ## Add supplementary quantitative variables
    if (!is.null(clean$extra)) set_extra(results) <- clean$extra

    results
  }
)

#' @export
#' @rdname pca
#' @aliases pca,matrix-method
setMethod(
  f = "pca",
  signature = c(object = "matrix"),
  definition = function(object, center = TRUE, scale = TRUE, rank = NULL,
                        sup_row = NULL, sup_col = NULL,
                        weight_row = NULL, weight_col = NULL) {
    # Fix dimension names
    names_row <- rownames(object)
    names_col <- colnames(object)
    if (is.null(names_row)) names_row <- as.character(seq_len(nrow(object)))
    if (is.null(names_col)) names_col <- as.character(seq_len(ncol(object)))

    # Subset
    is_row_sup <- find_variable(sup_row, nrow(object), names = rownames(object))
    is_col_sup <- find_variable(sup_col, ncol(object), names = colnames(object))
    N <- object[!is_row_sup, !is_col_sup, drop = FALSE]

    ## Check missing values
    arkhe::assert_missing(N)

    ## Check dimensions
    arkhe::assert_filled(N)

    # Dimension of the solution
    ndim <- min(rank, dim(N) - 1)
    i <- nrow(N)
    j <- ncol(N)

    # Weights
    w_row <- if (is.null(weight_row)) rep(1, nrow(N)) else weight_row
    w_col <- if (is.null(weight_col)) rep(1, ncol(N)) else weight_col
    w_row <- w_row / sum(w_row)

    # Build matrix
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
    D <- svd2(S, ndim)
    sv <- D$d # Singular values

    # Standard coordinates
    U <- D$u / W_row2
    V <- D$v / W_col2

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

    # names(sv) <- paste0("F", dim_keep)
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
