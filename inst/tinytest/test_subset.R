# Correspondence Analysis ======================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
res <- ca(cts)

expect_null(res[["X"]])
expect_inherits(res[["data"]], "matrix")
expect_length(res[["data"]], 100)
expect_inherits(res[["rows"]], "list")
expect_length(res[["rows"]], 5)
expect_inherits(res[["columns"]], "list")
expect_length(res[["columns"]], 5)
expect_inherits(res[["eigenvalues"]], "numeric")
expect_length(res[["eigenvalues"]], 4)

# Principal Components Analysis ================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
res <- pca(cts)

expect_null(res[["X"]])
expect_inherits(res[["data"]], "list")
expect_length(res[["data"]], 3)
expect_inherits(res[["rows"]], "list")
expect_length(res[["rows"]], 5)
expect_inherits(res[["columns"]], "list")
expect_length(res[["columns"]], 6)
expect_inherits(res[["eigenvalues"]], "numeric")
expect_length(res[["eigenvalues"]], 4)
