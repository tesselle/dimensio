# CORRESPONDENCE ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL


#' @export
#' @rdname ca
#' @aliases ca,data.frame-method
setMethod(
  f = "ca",
  signature = signature(object = "data.frame"),
  definition = function(object, n = NULL, sup_row = NULL, sup_col = NULL) {
    object <- as.matrix(object)
    ca(object = object, n = n, sup_row = sup_row, sup_col = sup_col)
  }
)

#' @export
#' @rdname ca
#' @aliases ca,matrix-method
setMethod(
  f = "ca",
  signature = signature(object = "matrix"),
  definition = function(object, n = NULL, sup_row = NULL, sup_col = NULL) {
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
    ndim <- min(n, dim(N) - 1)
    keep_dim <- seq_len(ndim)

    # Grand total
    total <- sum(N, na.rm = FALSE)
    # Relative frequencies
    P <- N / total

    # Calcul des marges
    w_row <- rowSums(P, na.rm = FALSE)
    w_col <- colSums(P, na.rm = FALSE)
    W_row <- sqrt(diag(1 / w_row))
    W_col <- sqrt(diag(1 / w_col))

    # /!\ Important: we need to clean the data before processing
    # Empty rows/columns must be removed to avoid error in svd()
    if (any(w_row == 0))
      stop("Empty rows detected.", call. = FALSE)
    if (any(w_col == 0))
      stop("Empty columns detected.", call. = FALSE)

    # Calcul des écarts à l'indépendance
    M <- P - tcrossprod(w_row, w_col)

    # Matrix of standardized residuals
    S <- W_row %*% M %*% W_col

    # Singular Value Decomposition
    D <- svd(S)
    sv <- D$d[keep_dim] # Singular values

    # Standard coordinates
    U <- W_row %*% D$u[, keep_dim, drop = FALSE]
    V <- W_col %*% D$v[, keep_dim, drop = FALSE]

    # Set names
    colnames(U) <- colnames(V) <- names(sv) <- paste0("PC", keep_dim)

    # Principal coordinates
    coord_row <- U %*% diag(sv)
    coord_col <- V %*% diag(sv)

    # Squared distance to centroide
    dist_row <- rowSums(S^2) / w_row
    dist_col <- colSums(S^2) / w_col

    # Supplementary points
    if (any(is_row_sup)) {
      extra_row <- object[is_row_sup, !is_col_sup, drop = FALSE]
      row_sup <- t(extra_row / rowSums(extra_row))

      # Coordinates
      coord_row_sup <- crossprod(row_sup, V)
      coord_row <- rbind(coord_row, coord_row_sup)

      # Distances
      dist_row_sup <- colSums((row_sup - w_col)^2 / w_col)
      dist_row <- c(dist_row, dist_row_sup)
    }
    if (any(is_col_sup)) {
      extra_col <- object[!is_row_sup, is_col_sup, drop = FALSE]
      col_sup <- t(t(extra_col) / colSums(extra_col))

      # Coordinates
      coord_col_sup <- crossprod(col_sup, U)
      coord_col <- rbind(coord_col, coord_col_sup)

      # Distances
      dist_col_sup <- colSums((col_sup - w_row)^2 / w_row)
      dist_col <- c(dist_col, dist_col_sup)
    }

    # Squared cosine
    cos_row <- coord_row^2 / dist_row
    cos_col <- coord_col^2 / dist_col

    .CA(
      data = object,
      dimension = as.integer(ndim),
      row_names = names_row,
      row_coordinates = coord_row,
      row_standard = U,
      row_distances = dist_row,
      row_cosine = cos_row,
      row_weights = w_row,
      row_supplement = is_row_sup,
      column_names = names_col,
      column_coordinates = coord_col,
      column_standard = V,
      column_distances = dist_col,
      column_cosine = cos_col,
      column_weights = w_col,
      column_supplement = is_col_sup,
      singular_values = sv
    )
  }
)

#' @export
#' @rdname predict
#' @aliases predict,CA-method
setMethod(
  f = "predict",
  signature = signature(object = "CA"),
  definition = function(object, newdata, margin = 1) {
    # Coerce to matrix
    data <- if (missing(newdata)) object@data else as.matrix(newdata)

    # TODO: keep only matching rows/columns

    # Get standard coordinates
    if (margin == 1) {
      data <- data / rowSums(data)
      std <- object@column_standard
    }
    if (margin == 2) {
      data <- t(data) / colSums(data)
      std <- object@row_standard
    }

    # Compute principal coordinates
    coords <- crossprod(t(data), std)
    coords <- as.data.frame(coords)
    colnames(coords) <- paste0("CA", seq_along(coords))
    return(coords)
  }
)
