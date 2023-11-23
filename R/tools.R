# TOOLS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname cdt
#' @aliases cdt,matrix-method
setMethod(
  f = "cdt",
  signature = c(object = "matrix"),
  definition = function(object, exclude = NULL, abbrev = TRUE) {
    ## Fix colnames
    if (is.null(colnames(object))) {
      colnames(object) <- paste0("V", seq_len(ncol(object)))
    }

    d <- apply(
      X = object,
      MARGIN = 2,
      FUN = function(cl, exclude) {
        cl <- factor(x = cl, exclude = exclude)
        n <- length(cl)
        z <- matrix(0, nrow = n, ncol = nlevels(cl))
        z[seq_len(n) + n * (unclass(cl) - 1)] <- 1
        dimnames(z) <- list(names(cl), levels(cl))
        z
      },
      exclude = exclude,
      simplify = FALSE
    )
    mtx <- do.call(cbind, d)

    if (!abbrev) {
      n <- vapply(X = d, FUN = ncol, FUN.VALUE = integer(1))
      colnames(mtx) <- paste(rep(colnames(object), n), colnames(mtx), sep = "_")
    }

    mtx
  }
)

#' @export
#' @rdname cdt
#' @aliases cdt,data.frame-method
setMethod(
  f = "cdt",
  signature = c(object = "data.frame"),
  definition = function(object, exclude = NULL, abbrev = TRUE) {
    object <- as.matrix(object)
    methods::callGeneric(object, exclude = exclude, abbrev = abbrev)
  }
)

#' @export
#' @rdname burt
#' @aliases burt,data.frame-method
setMethod(
  f = "burt",
  signature = c(object = "data.frame"),
  definition = function(object, exclude = NULL, abbrev = TRUE) {
    x <- cdt(object, exclude = exclude, abbrev = abbrev)
    crossprod(x, x)
  }
)
