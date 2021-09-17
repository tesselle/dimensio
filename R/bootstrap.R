# BOOTSTRAP
#' @include AllClasses.R AllGenerics.R
NULL

# numeric/integer ==============================================================
#' @export
#' @describeIn bootstrap Samples randomly from the elements of `object` with
#'  replacement.
#' @aliases bootstrap,numeric-method
setMethod(
  f = "bootstrap",
  signature = c(object = "numeric"),
  definition = function(object, do, n, ...) {
    spl <- sample(object, size = length(object) * n, replace = TRUE)
    replicates <- t(matrix(spl, nrow = n))
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    .BootstrapVector(values)
  }
)

#' @export
#' @describeIn bootstrap Samples observations from a multinomial distribution.
#' @aliases bootstrap,integer-method
setMethod(
  f = "bootstrap",
  signature = c(object = "integer"),
  definition = function(object, do, n, ...) {
    size <- sum(object)
    replicates <- stats::rmultinom(n, size = size, prob = object / size)
    values <- apply(X = replicates, MARGIN = 2, FUN = do, ...)
    .BootstrapVector(values)
  }
)

#' @export
#' @rdname bootstrap
#' @aliases summary,BootstrapVector-method
setMethod(
  f = "summary",
  signature = c(object = "BootstrapVector"),
  definition = function(object, level = 0.95, type = c("student", "normal"),
                        probs = c(0.25, 0.75), na.rm = FALSE, ...) {
    ## Confidence interval for the mean
    CI <- conf <- NULL
    if (!is.null(level)) {
      CI <- confidence_mean(object, level = level, type = type)
      conf <- c("lower", "upper")
    }

    ## Quantiles
    QU <- quant <- NULL
    if (!is.null(probs)) {
      QU <- stats::quantile(object, probs = probs, na.rm = na.rm, names = FALSE)
      quant <- sprintf("Q%02d", round(probs * 100, 0))
    }

    results <- c(
      min(object, na.rm = na.rm),
      mean(object, na.rm = na.rm),
      max(object, na.rm = na.rm),
      CI,
      QU
    )
    names(results) <- c("min", "mean", "max", conf, quant)
    results
  }
)

#' Confidence Interval for a Mean
#'
#' Computes the margin of errors of a confidence interval at a desired level of
#'  significance.
#' @param x A [`numeric`] vector.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#'  Must be a single number between \eqn{0} and \eqn{1}.
#' @param type A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`student`" (default) or
#'  "`normal`". Any unambiguous substring can be given.
#' @return A length-two [`numeric`] vector giving the margins of errors.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
confidence_mean <- function(x, level = 0.95, type = c("student", "normal")) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)

  n <- length(x)
  z <- zscore(alpha = 1 - level, n = n, type = type)
  stardard_error <- stats::sd(x) / sqrt(n)

  margin <- z * stardard_error
  mean(x) + margin * c(-1, 1)
}

zscore <- function(alpha, n, type = c("student", "normal")) {
  switch(
    type,
    normal = stats::qnorm(1 - alpha / 2), # Large sample size
    student = stats::qt(1 - alpha / 2, n - 1), # Small sample size
    stop(sprintf("There is no such type: %s", type), call. = FALSE)
  )
}

# MultivariateAnalysis =========================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,CA-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "CA"),
  definition = function(object, n = 30) {
    ## Data replication
    n <- as.integer(n)
    data <- object@data
    data <- data[!object@rows@supplement, !object@columns@supplement]
    repl <- stats::rmultinom(n = n, size = sum(data), prob = data)

    i <- nrow(data)
    j <- ncol(data)

    k_n <- seq_len(n)
    k_i <- seq_len(i)
    k_j <- seq_len(j)

    new_row <- matrix(data = NA_integer_, nrow = i * n, ncol = j)
    new_col <- matrix(data = NA_integer_, nrow = i, ncol = j * n)
    for (p in k_n) {
      m_i <- k_i + i * (p - 1)
      m_j <- k_j + j * (p - 1)
      new_row[m_i, ] <- repl[, p]
      new_col[, m_j] <- repl[, p]
    }

    res_row <- ca(rbind(data, new_row), sup_row = 1:(i * n) + i)
    res_col <- ca(cbind(data, new_col), sup_col = 1:(j * n) + j)

    ## Set names
    names_row <- rep_len(object@rows@names, i * (n + 1))
    names_col <- rep_len(object@columns@names, j * (n + 1))
    res_row@rows@names <- make.unique(names_row, sep = "_")
    res_col@columns@names <- make.unique(names_col, sep = "_")

    ## Set groups
    res_row@rows@groups <- names_row
    res_col@columns@groups <- names_col

    .BootstrapCA(
      object,
      rows = res_row@rows,
      columns = res_col@columns,
      replications = n
    )
  }
)

#' @export
#' @rdname bootstrap
#' @aliases bootstrap,PCA-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "PCA"),
  definition = function(object, n = 30) {
    ## Get data
    n <- as.integer(n)
    data <- object@data
    data <- data[!object@rows@supplement, !object@columns@supplement]
    U <- object@rows@standard
    w <- object@rows@weights
    i <- nrow(data)
    j <- ncol(data)

    k_n <- seq_len(n)
    k_i <- seq_len(i)
    k_j <- seq_len(j)

    ## Data replication
    new_coord <- matrix(data = NA_integer_, nrow = j * n, ncol = ncol(U))
    new_dist <- vector(mode = "numeric", length = j * n)
    for (p in k_n) {
      m_j <- k_j + j * (p - 1)
      z <- sample(i, size = i, replace = TRUE)
      w_i <- w[z]
      new_data <- data[z, ]

      ## Principal coordinates
      # Center and scale
      if (is_centered(object)) {
        new_data <- t(t(new_data) - weighted_mean(new_data, w_i))
      }
      if (is_scaled(object)) {
        new_data <- t(t(new_data) / weighted_sd(new_data, w_i))
      }
      var_sup <- new_data * w_i
      new_coord[m_j, ] <- crossprod(var_sup, U[z, ])

      ## Squared distance to centroide
      new_dist[m_j] <- colSums(new_data^2 * w_i)
    }

    ## Squared cosine
    new_cos <- new_coord^2 / new_dist

    ## Set names
    names_col <- rep_len(object@columns@names, j * (n + 1))

    new_col <- .MultivariateResults(
      object@columns,
      names = make.unique(names_col, sep = "_"),
      principal = rbind(object@columns@principal, new_coord),
      cosine = rbind(object@columns@cosine, new_cos),
      distances = c(object@columns@distances, new_dist),
      supplement = c(object@columns@supplement, !logical(j * n)),
      groups = names_col
    )
    .BootstrapPCA(
      object,
      columns = new_col,
      replications = n
    )
  }
)
