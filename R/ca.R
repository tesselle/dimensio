# CORRESPONDENCE ANALYSIS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ca
#' @aliases ca,data.frame-method
setMethod(
  f = "ca",
  signature = c(object = "data.frame"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL) {
    ## Remove non-numeric variables, if any
    clean <- drop_variable(object, f = is.numeric, negate = TRUE,
                           sup = sup_col, extra = NULL, what = "qualitative")

    object <- as.matrix(clean$data)
    methods::callGeneric(object = object, rank = rank,
                         sup_row = sup_row, sup_col = clean$sup)
  }
)

#' @export
#' @rdname ca
#' @aliases ca,matrix-method
setMethod(
  f = "ca",
  signature = c(object = "matrix"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL) {
    ## Fix dimension names
    names_row <- rownames(object)
    names_col <- colnames(object)
    if (is.null(names_row)) names_row <- as.character(seq_len(nrow(object)))
    if (is.null(names_col)) names_col <- as.character(seq_len(ncol(object)))

    ## Subset
    is_row_sup <- find_variable(sup_row, nrow(object), names = rownames(object))
    is_col_sup <- find_variable(sup_col, ncol(object), names = colnames(object))
    N <- object[!is_row_sup, !is_col_sup, drop = FALSE]

    ## Check missing values
    arkhe::assert_missing(N)

    ## Check dimensions
    arkhe::assert_filled(N)

    ## Dimension of the solution
    ndim <- min(rank, dim(N) - 1)
    i <- nrow(N)
    j <- ncol(N)

    ## Grand total
    total <- sum(N, na.rm = FALSE)
    ## Relative frequencies
    P <- N / total

    ## Calcul des marges
    w_row <- rowSums(P, na.rm = FALSE)
    w_col <- colSums(P, na.rm = FALSE)

    ## /!\ Important: we need to clean the data before processing
    ## Empty rows/columns must be removed to avoid error in svd()
    if (any(w_row == 0))
      stop(gettext("Empty rows detected."), call. = FALSE)
    if (any(w_col == 0))
      stop(gettext("Empty columns detected."), call. = FALSE)

    ## Build matrix
    ## matrix * vector is faster (!) than:
    # matrix %*% t(vector)
    # t(t(matrix) * vector)
    s_row <- sqrt(w_row)
    s_col <- sqrt(w_col)
    W_row1 <- matrix(s_row, nrow = i, ncol = j, byrow = FALSE)
    W_col1 <- matrix(s_col, nrow = i, ncol = j, byrow = TRUE)
    W_row2 <- matrix(s_row, nrow = i, ncol = ndim, byrow = FALSE)
    W_col2 <- matrix(s_col, nrow = j, ncol = ndim, byrow = FALSE)

    ## Calcul des écarts à l'indépendance
    M <- P - tcrossprod(w_row, w_col)

    ## Matrix of standardized residuals
    S <- M / W_row1 / W_col1

    ## Singular Value Decomposition
    D <- svd2(S, ndim)
    sv <- D$d # Singular values

    ## Standard coordinates
    U <- D$u / W_row2
    V <- D$v / W_col2

    sv_U <- matrix(sv, nrow = i, ncol = ndim, byrow = TRUE)
    sv_V <- matrix(sv, nrow = j, ncol = ndim, byrow = TRUE)

    ## Principal coordinates
    coord_row <- U * sv_U
    coord_col <- V * sv_V

    ## Contributions
    contrib_row <- ((coord_row * W_row2) / sv_U)^2 * 100
    contrib_col <- ((coord_col * W_col2) / sv_V)^2 * 100

    ## Squared distance to centroide
    dist_row <- rowSums(S^2) / w_row
    dist_col <- colSums(S^2) / w_col

    ## Supplementary points
    if (any(is_row_sup)) {
      extra_row <- object[is_row_sup, !is_col_sup, drop = FALSE]
      row_sup <- t(extra_row / rowSums(extra_row))

      ## Coordinates
      coord_row_sup <- crossprod(row_sup, V)
      coord_row <- rbind(coord_row, coord_row_sup)

      ## Distances
      dist_row_sup <- colSums((row_sup - w_col)^2 / w_col)
      dist_row <- c(dist_row, dist_row_sup)
    }
    if (any(is_col_sup)) {
      extra_col <- object[!is_row_sup, is_col_sup, drop = FALSE]
      col_sup <- t(t(extra_col) / colSums(extra_col))

      ## Coordinates
      coord_col_sup <- crossprod(col_sup, U)
      coord_col <- rbind(coord_col, coord_col_sup)

      ## Distances
      dist_col_sup <- colSums((col_sup - w_row)^2 / w_row)
      dist_col <- c(dist_col, dist_col_sup)
    }

    ## Squared cosine
    cos_row <- coord_row^2 / dist_row
    cos_col <- coord_col^2 / dist_col

    .CA(
      data = object,
      dimension = as.integer(ndim),
      singular_values = sv,
      rows = build_results(
        names = names_row,
        principal = coord_row,
        standard = U,
        contributions = contrib_row,
        distances = dist_row,
        cosine = cos_row,
        weights = w_row,
        supplement = is_row_sup
      ),
      columns = build_results(
        names = names_col,
        principal = coord_col,
        standard = V,
        contributions = contrib_col,
        distances = dist_col,
        cosine = cos_col,
        weights = w_col,
        supplement = is_col_sup
      )
    )
  }
)
