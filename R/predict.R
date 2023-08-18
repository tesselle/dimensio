# PREDICT
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @rdname predict
#' @aliases predict,CA-method
setMethod(
  f = "predict",
  signature = signature(object = "CA"),
  definition = function(object, newdata, margin = 1) {
    # Coerce to matrix
    if (missing(newdata)) {
      data <- object@data
      data <- data[!object@rows@supplement, !object@columns@supplement]
    } else {
      data <- as.matrix(newdata)
    }

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
    colnames(coords) <- paste0("F", seq_along(coords))
    return(coords)
  }
)

# PCA ==========================================================================
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
