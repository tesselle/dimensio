test_that("PCA", {
  data("countries")

  row_zeros <- countries
  row_zeros[1, ] <- NA
  expect_error(pca(row_zeros), "Missing values detected.")

  expect_error(pca(countries, sup_row = "row1"), "must be a numeric vector")
  expect_error(pca(countries, sup_col = "col1"), "must be a numeric vector")

  res <- pca(countries, center = TRUE, scale = FALSE, rank = 5)
  expect_output(show(res), "Principal Components Analysis")
  expect_equal(rownames(res), rownames(countries))
  expect_equal(colnames(res), colnames(countries))
  expect_equal(dimnames(res), dimnames(countries))

  expect_equal(get_data(res), countries)

  # Points coordinates
  expect_snapshot(get_coordinates(res, margin = 1, principal = TRUE))
  expect_snapshot(get_coordinates(res, margin = 2, principal = TRUE))
  expect_snapshot(get_coordinates(res, margin = 1, principal = FALSE))
  expect_snapshot(get_coordinates(res, margin = 2, principal = FALSE))

  # Tidy coordinates
  expect_snapshot(tidy(res, margin = 1))
  expect_snapshot(tidy(res, margin = 2))

  # Distances
  expect_snapshot(get_distances(res, margin = 1))
  expect_snapshot(get_distances(res, margin = 2))

  # Inertias
  expect_snapshot(get_inertia(res, margin = 1))
  expect_snapshot(get_inertia(res, margin = 2))

  # Eigenvalues
  expect_snapshot(get_eigenvalues(res))
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
