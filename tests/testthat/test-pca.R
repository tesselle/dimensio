test_that("PCA - matrix", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)

  row_zeros <- cts
  row_zeros[1, ] <- NA
  expect_error(pca(row_zeros), "Missing values detected.")

  expect_error(pca(cts, sup_row = "row1"), "must be a numeric vector")
  expect_error(pca(cts, sup_col = "col1"), "must be a numeric vector")

  res <- pca(cts, center = TRUE, scale = TRUE, rank = 10)
  expect_output(show(res), "Principal Components Analysis")
  expect_equal(rownames(res), as.character(seq_len(20)))
  expect_equal(colnames(res), as.character(seq_len(5)))
  expect_equal(dimnames(res), list(as.character(seq_len(20)),
                                   as.character(seq_len(5))))

  expect_equal(get_data(res), as.data.frame(cts))

  # Points coordinates
  coord_row <- get_coordinates(res, margin = 1)
  expect_equal(dim(coord_row), c(20L, 5L))
  coord_col <- get_coordinates(res, margin = 2)
  expect_equal(dim(coord_col), c(5L, 5L))

  # Distances
  dist_row <- get_distances(res, margin = 1)
  expect_length(dist_row, 20)
  dist_col <- get_distances(res, margin = 2)
  expect_length(dist_col, 5)

  # Inertias
  inertia_row <- get_inertia(res, margin = 1)
  expect_length(inertia_row, 20)
  inertia_col <- get_inertia(res, margin = 2)
  expect_length(inertia_col, 5)

  # Eigenvalues
  eig <- get_eigenvalues(res)
  expect_equal(dim(eig), c(4L, 3L))
})
test_that("PCA - data.frame", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 20)
  df <- as.data.frame(cts)
  df$test <- character(5)

  expect_message(pca(df, sup_col = 1:5), "qualitative variable was removed")
})
test_that("Predict new coordinates", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)

  res <- pca(cts, center = FALSE, scale = FALSE)
  new_rows <- predict(res, cts, margin = 1)
  new_cols <- predict(res, cts, margin = 2)

  sup_rows <- get_coordinates(res, margin = 1)
  sup_cols <- get_coordinates(res, margin = 2)

  expect_equal(new_rows, sup_rows[, 1:4], ignore_attr = TRUE)
  expect_equal(new_cols, sup_cols[, 1:4], ignore_attr = TRUE)
})
