# BOOTSTRAP
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname bootstrap
#' @aliases bootstrap,CA-method
setMethod(
  f = "bootstrap",
  signature = signature(object = "CA"),
  definition = function(object, n = 30) {
    ## Data replication
    data <- object@data
    repl <- stats::rmultinom(n = n, size = sum(data), prob = data)

    i <- nrow(data)
    j <- ncol(data)

    k_n <- seq_len(n)
    k_i <- seq_len(i)
    k_j <- seq_len(j)

    new_row <- matrix(data = NA_integer_, nrow = i * n, ncol = j)
    new_col <- matrix(data = NA_integer_, nrow = i, ncol = j * n)
    for (p in k_n) {
      m1 <- k_i + i * (p - 1)
      m2 <- k_j + j * (p - 1)
      new_row[m1, ] <- repl[, p]
      new_col[, m2] <- repl[, p]
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

    methods::initialize(
      object,
      rows = res_row@rows,
      columns = res_col@columns
    )
  }
)
