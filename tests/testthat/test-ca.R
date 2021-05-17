test_that("CA - matrix", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)

  row_zeros <- col_zeros <- cts
  row_zeros[1, ] <- 0
  expect_error(ca(row_zeros), "Empty rows detected.")
  col_zeros[, 1] <- 0
  expect_error(ca(col_zeros), "Empty columns detected.")

  expect_error(ca(cts, sup_row = "row1"), "must be a numeric vector")
  expect_error(ca(cts, sup_col = "col1"), "must be a numeric vector")

  res <- ca(cts, rank = 10)
  expect_output(show(res), "Correspondence Analysis")
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
test_that("Compare with {FactoMineR}", {
  skip_on_cran()
  skip_if_not_installed("FactoMineR")

  mtx <- matrix(data = sample(1:10, 1000, TRUE), ncol = 10)
  df <- as.data.frame(mtx)

  is_sup_rows <- sort(sample(1:10, 3, FALSE))
  is_sup_cols <- sort(sample(1:10, 4, FALSE))

  res_facto <- FactoMineR::CA(df, row.sup = is_sup_rows, col.sup = is_sup_cols,
                              graph = FALSE)
  res_arkhe <- ca(df, sup_row = is_sup_rows, sup_col = is_sup_cols)

  # Get coordinates
  coord_row <- get_coordinates(res_arkhe, margin = 1)
  coord_col <- get_coordinates(res_arkhe, margin = 2)

  # Row principal coordinates
  expect_equal(
    object = abs(as.data.frame(res_facto$row$coord)),
    expected = abs(coord_row[!coord_row$.sup, -ncol(coord_row)]),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$coord),
    rownames(coord_row[!coord_row$.sup, -ncol(coord_row)])
  )

  # Supplementary row coordinates
  expect_equal(
    object = abs(as.data.frame(res_facto$row.sup$coord)),
    expected = abs(coord_row[coord_row$.sup, -ncol(coord_row)]),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row.sup$coord),
    rownames(coord_row[coord_row$.sup, -ncol(coord_row)])
  )

  # Column principal coordinates
  expect_equal(
    object = abs(as.data.frame(res_facto$col$coord)),
    expected = abs(coord_col[!coord_col$.sup, -ncol(coord_col)]),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$coord),
    rownames(coord_col[!coord_col$.sup, -ncol(coord_col)])
  )

  # Supplementary column coordinates
  expect_equal(
    object = abs(as.data.frame(res_facto$col.sup$coord)),
    expected = abs(coord_col[coord_col$.sup, -ncol(coord_col)]),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col.sup$coord),
    rownames(coord_col[coord_col$.sup, -ncol(coord_col)])
  )

  # Row contributions
  expect_equal(
    object = as.data.frame(res_facto$row$contrib),
    expected = get_contributions(res_arkhe, margin = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$contrib),
    rownames(get_contributions(res_arkhe, margin = 1))
  )

  # Column contributions
  expect_equal(
    object = as.data.frame(res_facto$col$contrib),
    expected = get_contributions(res_arkhe, margin = 2),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$contrib),
    rownames(get_contributions(res_arkhe, margin = 2))
  )

  # Row inertias
  expect_equal(
    object = res_facto$row$inertia,
    expected = get_inertia(res_arkhe, margin = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$inertia),
    rownames(get_inertia(res_arkhe, margin = 1))
  )

  # Column inertias
  expect_equal(
    object = res_facto$col$inertia,
    expected = get_inertia(res_arkhe, margin = 2),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$inertia),
    rownames(get_inertia(res_arkhe, margin = 2))
  )

  # Get cos2
  cos2_row <- get_cos2(res_arkhe, margin = 1)
  cos2_col <- get_cos2(res_arkhe, margin = 2)

  # Row cos2
  expect_equal(
    object = as.data.frame(res_facto$row$cos2),
    expected = cos2_row[!cos2_row$.sup, -ncol(cos2_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$cos2),
    rownames(cos2_row[!cos2_row$.sup, -ncol(cos2_row)])
  )

  # Supplementary row cos2
  expect_equal(
    object = as.data.frame(res_facto$row.sup$cos2),
    expected = cos2_row[cos2_row$.sup, -ncol(cos2_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row.sup$cos2),
    rownames(cos2_row[cos2_row$.sup, -ncol(cos2_row)])
  )

  # Column cos2
  expect_equal(
    object = as.data.frame(res_facto$col$cos2),
    expected = cos2_col[!cos2_col$.sup, -ncol(cos2_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$cos2),
    rownames(cos2_col[!cos2_col$.sup, -ncol(cos2_col)])
  )

  # Supplementary column cos2
  expect_equal(
    object = as.data.frame(res_facto$col.sup$cos2),
    expected = cos2_col[cos2_col$.sup, -ncol(cos2_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col.sup$cos2),
    rownames(cos2_col[cos2_col$.sup, -ncol(cos2_col)])
  )
})
