test_that("CA", {
  skip_on_cran()
  skip_if_not_installed("FactoMineR")

  mtx <- matrix(data = sample(1:10, 1000, TRUE), ncol = 10)
  df <- as.data.frame(mtx)

  is_sup_rows <- sort(sample(1:10, 3, FALSE))
  is_sup_cols <- sort(sample(1:10, 4, FALSE))

  res_facto <- FactoMineR::CA(df, row.sup = is_sup_rows, col.sup = is_sup_cols,
                              graph = FALSE)
  res_dim <- ca(df, sup_row = is_sup_rows, sup_col = is_sup_cols)

  # Get coordinates
  coord_row <- get_coordinates(res_dim, margin = 1)
  coord_col <- get_coordinates(res_dim, margin = 2)

  # Row principal coordinates
  expect_equal(
    object = as.data.frame(res_facto$row$coord),
    expected = coord_row[!coord_row$.sup, -ncol(coord_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$coord),
    rownames(coord_row[!coord_row$.sup, -ncol(coord_row)])
  )

  # Supplementary row coordinates
  expect_equal(
    object = as.data.frame(res_facto$row.sup$coord),
    expected = coord_row[coord_row$.sup, -ncol(coord_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row.sup$coord),
    rownames(coord_row[coord_row$.sup, -ncol(coord_row)])
  )

  # Column principal coordinates
  expect_equal(
    object = as.data.frame(res_facto$col$coord),
    expected = coord_col[!coord_col$.sup, -ncol(coord_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$coord),
    rownames(coord_col[!coord_col$.sup, -ncol(coord_col)])
  )

  # Supplementary column coordinates
  expect_equal(
    object = as.data.frame(res_facto$col.sup$coord),
    expected = coord_col[coord_col$.sup, -ncol(coord_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col.sup$coord),
    rownames(coord_col[coord_col$.sup, -ncol(coord_col)])
  )

  # Row contributions
  expect_equal(
    object = as.data.frame(res_facto$row$contrib),
    expected = get_contributions(res_dim, margin = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$contrib),
    rownames(get_contributions(res_dim, margin = 1))
  )

  # Column contributions
  expect_equal(
    object = as.data.frame(res_facto$col$contrib),
    expected = get_contributions(res_dim, margin = 2),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$contrib),
    rownames(get_contributions(res_dim, margin = 2))
  )

  # Row inertias
  expect_equal(
    object = res_facto$row$inertia,
    expected = get_inertia(res_dim, margin = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$row$inertia),
    rownames(get_inertia(res_dim, margin = 1))
  )

  # Column inertias
  expect_equal(
    object = res_facto$col$inertia,
    expected = get_inertia(res_dim, margin = 2),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$col$inertia),
    rownames(get_inertia(res_dim, margin = 2))
  )

  # Get cos2
  cos2_row <- get_cos2(res_dim, margin = 1)
  cos2_col <- get_cos2(res_dim, margin = 2)

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
test_that("PCA", {
  skip_on_cran()
  skip_if_not_installed("FactoMineR")

  mtx <- matrix(data = sample(1:100, 1000, TRUE), ncol = 10)
  df <- as.data.frame(mtx)

  is_sup_rows <- sort(sample(1:10, 3, FALSE))
  is_sup_cols <- sort(sample(1:10, 4, FALSE))

  res_facto <- FactoMineR::PCA(df, scale.unit = TRUE, ind.sup = is_sup_rows,
                               quanti.sup = is_sup_cols, graph = FALSE)
  res_dim <- pca(df, scale = TRUE, sup_row = is_sup_rows,
                 sup_col = is_sup_cols)

  # Get coordinates
  coord_row <- get_coordinates(res_dim, margin = 1)
  coord_col <- get_coordinates(res_dim, margin = 2)

  # Row principal coordinates
  expect_equal(
    object = as.data.frame(res_facto$ind$coord),
    expected = coord_row[!coord_row$.sup, -ncol(coord_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$ind$coord),
    rownames(coord_row[!coord_row$.sup, -ncol(coord_row)])
  )

  # Supplementary row coordinates
  expect_equal(
    object = as.data.frame(res_facto$ind.sup$coord),
    expected = coord_row[coord_row$.sup, -ncol(coord_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$ind.sup$coord),
    rownames(coord_row[coord_row$.sup, -ncol(coord_row)])
  )

  # Column principal coordinates
  expect_equal(
    object = as.data.frame(res_facto$var$coord),
    expected = coord_col[!coord_col$.sup, -ncol(coord_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$var$coord),
    rownames(coord_col[!coord_col$.sup, -ncol(coord_col)])
  )

  # Supplementary column coordinates
  expect_equal(
    object = as.data.frame(res_facto$quanti.sup$coord),
    expected = coord_col[coord_col$.sup, -ncol(coord_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$quanti.sup$coord),
    rownames(coord_col[coord_col$.sup, -ncol(coord_col)])
  )

  # Row contributions
  expect_equal(
    object = as.data.frame(res_facto$ind$contrib),
    expected = get_contributions(res_dim, margin = 1),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$ind$contrib),
    rownames(get_contributions(res_dim, margin = 1))
  )

  # Column contributions
  expect_equal(
    object = as.data.frame(res_facto$var$contrib),
    expected = get_contributions(res_dim, margin = 2),
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$var$contrib),
    rownames(get_contributions(res_dim, margin = 2))
  )

  # Column correlations
  cor_col <- get_correlations(res_dim)
  expect_equal(
    object = as.data.frame(res_facto$var$cor),
    expected = cor_col[!cor_col$.sup, -ncol(cor_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$var$cor),
    rownames(cor_col[!cor_col$.sup, -ncol(cor_col)])
  )

  # Get cos2
  cos2_row <- get_cos2(res_dim, margin = 1)
  cos2_col <- get_cos2(res_dim, margin = 2)

  # Row cos2
  expect_equal(
    object = as.data.frame(res_facto$ind$cos2),
    expected = cos2_row[!cos2_row$.sup, -ncol(cos2_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$ind$cos2),
    rownames(cos2_row[!cos2_row$.sup, -ncol(cos2_row)])
  )

  # Supplementary row cos2
  expect_equal(
    object = as.data.frame(res_facto$ind.sup$cos2),
    expected = cos2_row[cos2_row$.sup, -ncol(cos2_row)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$ind.sup$cos2),
    rownames(cos2_row[cos2_row$.sup, -ncol(cos2_row)])
  )

  # Column cos2
  expect_equal(
    object = as.data.frame(res_facto$var$cos2),
    expected = cos2_col[!cos2_col$.sup, -ncol(cos2_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$var$cos2),
    rownames(cos2_col[!cos2_col$.sup, -ncol(cos2_col)])
  )

  # Supplementary column cos2
  expect_equal(
    object = as.data.frame(res_facto$quanti.sup$cos2),
    expected = cos2_col[cos2_col$.sup, -ncol(cos2_col)],
    ignore_attr = TRUE
  )
  expect_equal(
    rownames(res_facto$quanti.sup$cos2),
    rownames(cos2_col[cos2_col$.sup, -ncol(cos2_col)])
  )
})
