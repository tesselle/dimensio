# JACKKNIFE
#' @include AllClasses.R AllGenerics.R
NULL

# Jaccknife ====================================================================
#' @export
#' @rdname jackknife
#' @aliases jackknife,numeric-method
setMethod(
  f = "jackknife",
  signature = c(object = "numeric"),
  definition = function(object, do, ...) {
    n <- length(object)
    hat <- do(object, ...)

    values <- vapply(
      X = seq_len(n),
      FUN = function(i, x, do, ...) {
        do(x[-i], ...)
      },
      FUN.VALUE = double(1),
      x = object, do = do, ...
    )
    .JackknifeVector(values, hat = hat)
  }
)

#' @export
#' @rdname jackknife
#' @aliases summary,JackknifeVector-method
setMethod(
  f = "summary",
  signature = c(object = "JackknifeVector"),
  definition = function(object, ...) {
    n <- length(object)
    jack_mean <- mean(object)
    jack_bias <- (n - 1) * (jack_mean - object@hat)
    jack_error <- sqrt(((n - 1) / n) * sum((object - jack_mean)^2))

    results <- c(jack_mean, jack_bias, jack_error)
    names(results) <- c("mean", "bias", "error")
    results
  }
)
