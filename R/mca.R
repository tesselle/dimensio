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
    ## Subset
    is_row_sup <- is_supplementary(sup_row, nrow(object), names = rownames(object))
    is_col_sup <- is_supplementary(sup_col, ncol(object), names = colnames(object))
    is_quanti_sup <- is_supplementary(sup_quanti, ncol(object), names = colnames(object))
    is_active <- !(is_col_sup | is_quanti_sup)
    N <- object[, is_active, drop = FALSE]

    ## Complete disjunctive table
    Z <- cdt(N)

    ## Get supplementary columns
    Z_tot <- Z
    sup_col <- NULL
    if (any(is_col_sup)) {
      Z_sup <- cdt(object[, is_col_sup, drop = FALSE])
      Z_tot <- cbind(Z, Z_sup)
      sup_col <- seq_len(ncol(Z_sup)) + ncol(Z)
    }

    ## Compute
    ndim <- min(rank, ncol(Z_tot) - sum(is_active))
    results <- ca(Z_tot, rank = ndim, sup_row = sup_row, sup_col = sup_col)

    .MCA(results)
  }
)
