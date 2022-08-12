test_that("CA", {
  data("benthos")

  row_zeros <- col_zeros <- benthos
  row_zeros[1, ] <- 0
  expect_error(ca(row_zeros), "Empty rows detected.")
  col_zeros[, 1] <- 0
  expect_error(ca(col_zeros), "Empty columns detected.")

  expect_error(ca(benthos, sup_row = "row1"), "must be a numeric vector")
  expect_error(ca(benthos, sup_col = "col1"), "must be a numeric vector")

  res <- ca(benthos, rank = 10)
  expect_output(show(res), "Correspondence Analysis")
  expect_equal(rownames(res), rownames(benthos))
  expect_equal(colnames(res), colnames(benthos))
  expect_equal(dimnames(res), dimnames(benthos))

  expect_equal(get_data(res), benthos)

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
test_that("CA - data.frame", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 20)
  df <- as.data.frame(cts)
  df$test <- character(5)

  expect_message(ca(df, sup_col = 1:5), "qualitative variable was removed")
})
test_that("Predict new coordinates", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)

  res <- ca(cts)
  new_rows <- predict(res, cts, margin = 1)
  new_cols <- predict(res, cts, margin = 2)

  sup_rows <- get_coordinates(res, margin = 1)
  sup_cols <- get_coordinates(res, margin = 2)

  expect_equal(new_rows, sup_rows[, 1:4], ignore_attr = TRUE)
  expect_equal(new_cols, sup_cols[, 1:4], ignore_attr = TRUE)
})
