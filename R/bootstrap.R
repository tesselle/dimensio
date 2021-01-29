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
    k <- seq_len(n)

    new_row <- matrix(data = NA_integer_, nrow = i * n, ncol = j)
    new_col <- matrix(data = NA_integer_, nrow = i, ncol = j * n)
    for (p in k) {
      m1 <- 1:i + i * (p - 1)
      m2 <- 1:j + j * (p - 1)
      new_row[m1, ] <- repl[, p]
      new_col[, m2] <- repl[, p]
    }

    res_row <- ca(rbind(data, new_row), sup_row = 1:(i * n) + i)
    res_col <- ca(cbind(data, new_col), sup_col = 1:(j * n) + j)

    methods::initialize(
      object,
      rows = res_row@rows,
      columns = res_col@columns
    )
  }
)
