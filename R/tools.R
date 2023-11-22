# TOOLS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname cdt
#' @aliases cdt,matrix-method
setMethod(
  f = "cdt",
  signature = c(object = "matrix"),
  definition = function(object, exclude = NULL) {
    ## Fix colnames
    if (is.null(colnames(object))) {
      colnames(object) <- paste0("V", seq_len(ncol(object)))
    }

    d <- lapply(
      X = seq_len(ncol(object)),
      FUN = function(i, x, exclude) {
        xi <- x[, i]
        xi <- factor(x = xi, levels = sort(unique(xi)), exclude = exclude)
        n <- length(xi)
        z <- matrix(0, nrow = n, ncol = nlevels(xi))
        z[seq_len(n) + n * (unclass(xi) - 1)] <- 1
        colnames(z) <- paste(colnames(x)[i], levels(xi), sep = "_")
        z
      },
      x = object,
      exclude = exclude
    )
    d <- do.call(cbind, d)
    rownames(d) <- rownames(object)
    d
  }
)

#' @export
#' @rdname cdt
#' @aliases cdt,data.frame-method
setMethod(
  f = "cdt",
  signature = c(object = "data.frame"),
  definition = function(object) {
    object <- as.matrix(object)
    methods::callGeneric(object)
  }
)

#' @export
#' @rdname burt
#' @aliases burt,data.frame-method
setMethod(
  f = "burt",
  signature = c(object = "data.frame"),
  definition = function(object) {
    x <- cdt(object)
    crossprod(x, x)
  }
)
