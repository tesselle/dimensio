# CORRESPONDENCE ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL


#' @export
#' @rdname ca
#' @aliases ca,data.frame-method
setMethod(
  f = "ca",
  signature = signature(object = "data.frame"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL) {
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
    methods::callGeneric(object = object, rank = rank,
                         sup_row = sup_row, sup_col = sup_col)
  }
)

#' @export
#' @rdname ca
#' @aliases ca,matrix-method
setMethod(
  f = "ca",
  signature = signature(object = "matrix"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL) {
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

    # Grand total
    total <- sum(N, na.rm = FALSE)
    # Relative frequencies
    P <- N / total

    # Calcul des marges
    w_row <- rowSums(P, na.rm = FALSE)
    w_col <- colSums(P, na.rm = FALSE)

    # Build matrix
    # matrix * vector is faster (!) than:
    # matrix %*% t(vector)
    # t(t(matrix) * vector)
    s_row <- sqrt(w_row)
    s_col <- sqrt(w_col)
    W_row1 <- matrix(s_row, nrow = i, ncol = j, byrow = FALSE)
    W_col1 <- matrix(s_col, nrow = i, ncol = j, byrow = TRUE)
    W_col2 <- matrix(s_col, nrow = j, ncol = j, byrow = FALSE)
    W_row3 <- matrix(s_row, nrow = i, ncol = ndim, byrow = FALSE)
    W_col3 <- matrix(s_col, nrow = j, ncol = ndim, byrow = FALSE)

    # /!\ Important: we need to clean the data before processing
    # Empty rows/columns must be removed to avoid error in svd()
    if (any(w_row == 0))
      stop("Empty rows detected.", call. = FALSE)
    if (any(w_col == 0))
      stop("Empty columns detected.", call. = FALSE)

    # Calcul des écarts à l'indépendance
    M <- P - tcrossprod(w_row, w_col)

    # Matrix of standardized residuals
    S <- M / W_row1 / W_col1

    # Singular Value Decomposition
    D <- svd(S)
    sv <- D$d[dim_keep] # Singular values

    # Standard coordinates
    U <- D$u / W_row1
    V <- D$v / W_col2
    U <- U[, dim_keep, drop = FALSE]
    V <- V[, dim_keep, drop = FALSE]

    sv_U <- matrix(sv, nrow = i, ncol = ndim, byrow = TRUE)
    sv_V <- matrix(sv, nrow = j, ncol = ndim, byrow = TRUE)

    # Principal coordinates
    coord_row <- U * sv_U
    coord_col <- V * sv_V

    # Contributions
    contrib_row <- ((coord_row * W_row3) / sv_U)^2 * 100
    contrib_col <- ((coord_col * W_col3) / sv_V)^2 * 100

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
      singular_values = sv,
      rows = .MultivariateResults(
        names = names_row,
        coordinates = coord_row,
        standard = U,
        contributions = contrib_row,
        distances = dist_row,
        cosine = cos_row,
        weights = w_row,
        supplement = is_row_sup,
        prefix = "CA"
      ),
      columns = .MultivariateResults(
        names = names_col,
        coordinates = coord_col,
        standard = V,
        contributions = contrib_col,
        distances = dist_col,
        cosine = cos_col,
        weights = w_col,
        supplement = is_col_sup,
        prefix = "CA"
      )
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
      std <- object@columns@standard
    }
    if (margin == 2) {
      data <- t(data) / colSums(data)
      std <- object@rows@standard
    }

    # Compute principal coordinates
    coords <- crossprod(t(data), std)
    coords <- as.data.frame(coords)
    colnames(coords) <- paste0("CA", seq_along(coords))
    return(coords)
  }
)
