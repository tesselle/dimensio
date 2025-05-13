# BOOTSTRAP
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,CA-method
setMethod(
  f = "bootstrap",
  signature = c(object = "CA"),
  definition = function(object, n = 30) {
    ## Data replication
    n <- as.integer(n)
    arkhe::assert_scalar(n, "integer")

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

# PCA ==========================================================================
#' @export
#' @rdname bootstrap
#' @aliases bootstrap,PCA-method
setMethod(
  f = "bootstrap",
  signature = c(object = "PCA"),
  definition = function(object, n = 30) {
    ## Get data
    n <- as.integer(n)
    arkhe::assert_scalar(n, "integer")

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

    new_col <- build_results(
      names = make.unique(names_col, sep = "_"),
      principal = rbind(object@columns@principal, new_coord),
      standard = object@columns@standard,
      contributions = object@columns@contributions,
      distances = c(object@columns@distances, new_dist),
      cosine = rbind(object@columns@cosine, new_cos),
      weights = object@columns@weights,
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
