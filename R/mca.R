# MULTIPLE CORRESPONDENCE ANALYSIS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname mca
#' @aliases mca,data.frame-method
setMethod(
  f = "mca",
  signature = c(object = "data.frame"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL,
                        sup_quanti = NULL) {
    ## Remove numeric variables, if any
    clean <- drop_variable(object, f = is.numeric, negate = FALSE,
                           sup = sup_col, extra = sup_quanti, what = "quantitative")

    ## Compute MCA
    object <- as.matrix(clean$data)
    results <- methods::callGeneric(object = object, rank = rank,
                                    sup_row = sup_row, sup_col = clean$sup)

    ## Add supplementary quantitative variables
    if (!is.null(sup_quanti)) set_extra(results) <- as.matrix(clean$extra)

    results
  }
)

#' @export
#' @rdname mca
#' @aliases mca,matrix-method
setMethod(
  f = "mca",
  signature = c(object = "matrix"),
  definition = function(object, rank = NULL, sup_row = NULL, sup_col = NULL) {
    ## Subset
    is_row_sup <- find_variable(sup_row, nrow(object), names = rownames(object))
    is_col_sup <- find_variable(sup_col, ncol(object), names = colnames(object))
    N <- object[, !is_col_sup, drop = FALSE]

    ## Complete disjunctive table
    Z <- cdt(N)

    ## Check missing values
    if (anyNA(Z)) stop("Missing values detected.", call. = FALSE)

    ## Get supplementary columns
    Z_tot <- Z
    sup_col <- NULL
    if (any(is_col_sup)) {
      Z_sup <- cdt(object[, is_col_sup, drop = FALSE])
      Z_tot <- cbind(Z, Z_sup)
      sup_col <- seq_len(ncol(Z_sup)) + ncol(Z)
    }

    ## Compute
    ndim <- min(rank, ncol(Z_tot) - sum(!is_col_sup))
    results <- ca(Z_tot, rank = ndim, sup_row = sup_row, sup_col = sup_col)

    .MCA(results)
  }
)
