# PRINCIPAL COORDINATES ANALYSIS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname pcoa
#' @aliases pcoa,dist-method
setMethod(
  f = "pcoa",
  signature = c(object = "dist"),
  definition = function(object, rank = 2) {
    ## Multidimensional scaling
    res <- stats::cmdscale(
      d = object,
      k = rank,
      eig = TRUE,
      add = FALSE,
      list. = TRUE
    )

    points <- res$points
    colnames(points) <- paste0("F", seq_len(NCOL(points)))

    .PCOA(points = points, eigenvalues = res$eig, GOF= res$GOF)
  }
)
