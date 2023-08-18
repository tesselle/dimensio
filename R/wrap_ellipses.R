# ELLIPSES
#' @include AllGenerics.R
NULL

# Confidence ===================================================================
#' @export
#' @rdname wrap
#' @aliases wrap_confidence,MultivariateAnalysis-method
setMethod(
  f = "wrap_confidence",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL,
                        level = 0.95) {
    ## Get coordinates
    data <- get_coordinates(x, margin = margin)
    data <- data[, axes]

    ## Add groups, if any
    k <- get_order(x, margin = margin)
    if (!is.null(group)) {
      assert_length(group, nrow(data))
      group <- group[k]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    } else {
      group <- rep("", length(k))
    }
    group <- as.character(group)

    ## Compute ellipse
    data <- split(data, f = group)
    lapply(
      X = data,
      FUN = function(x, level) {
        df1 <- ncol(x) - 1
        df2 <- nrow(x) - 2
        radius <- sqrt(stats::qf(p = level, df1, df2) * df1 / df2)
        wrap_ellipse(x[, 1], x[, 2], radius = radius)
      },
      level = level
    )
  }
)

# Tolerance ====================================================================
#' @export
#' @rdname wrap
#' @aliases wrap_tolerance,MultivariateAnalysis-method
setMethod(
  f = "wrap_tolerance",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL,
                        level = 0.95) {
    ## Get coordinates
    data <- get_coordinates(x, margin = margin)
    data <- data[, axes]

    ## Add groups, if any
    k <- get_order(x, margin = margin)
    if (!is.null(group)) {
      assert_length(group, nrow(data))
      group <- group[k]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    } else {
      group <- rep("", length(k))
    }
    group <- as.character(group)

    ## Compute ellipse
    data <- split(data, f = group)
    lapply(
      X = data,
      FUN = function(x, level) {
        df <- ncol(x) - 1
        radius <- sqrt(stats::qchisq(p = level, df = df))
        wrap_ellipse(x[, 1], x[, 2], radius = radius)
      },
      level = level
    )
  }
)

# Helpers ======================================================================
wrap_ellipse <- function(x, y, radius = 1) {
  ## Compute ellipse
  xy <- cbind(x, y)
  mu <- colMeans(xy)
  sigma <- stats::cov(xy)
  # rob <- robustbase::covMcd(xy)
  # mu <- rob$center
  # sigma <- rob$cov
  ellipse(sigma = sigma, mu = mu, radius = radius)
}

#' Computes an Ellipse
#'
#' @param sigma A square positive definite \eqn{2 \times 2}{2 x 2} covariance
#'  or correlation `matrix`.
#' @param mu A length-two [`numeric`] vector giving the centre of the ellipse.
#' @param scale If `sigma` is a correlation matrix, then the standard deviations
#'  of each parameter can be given in the scale parameter.
#'  Defaults to `c(1, 1)`, so no rescaling will be done.
#' @param level A length-\eqn{k} [`numeric`] vector giving the confidence level
#'  of a pairwise confidence region.
#' @param radius The size of the ellipse may also be controlled by specifying
#'  the value of a t-statistic on its boundary.
#' @param n A length-one [`numeric`] vector specifying the number of points used
#'  in the ellipse.
#' @note Adapted from [ellipse::ellipse()].
#' @return
#'  A [`list`] of \eqn{k} \eqn{n \times 2}{n x 2} `matrix`, suitable for
#'  plotting.
#' @keywords internal
#' @noRd
ellipse <- function(sigma, mu = c(0, 0), scale = c(1, 1), level = 0.95,
                    radius = sqrt(stats::qchisq(level, 2)), n = 100, ...) {
  r <- sigma[1, 2]

  if (missing(scale)) {
    scale <- sqrt(diag(sigma))
    if (scale[1] > 0) r <- r / scale[1]
    if (scale[2] > 0) r <- r / scale[2]
  }

  r <- min(max(r, -1), 1)  # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = n)

  lapply(
    X = radius,
    FUN = function(x) {
      matrix(
        data = c(x * scale[1] * cos(a + d / 2) + mu[1],
                 x * scale[2] * cos(a - d / 2) + mu[2]),
        nrow = n,
        ncol = 2,
        dimnames = list(NULL, c("x", "y"))
      )
    }
  )
}
