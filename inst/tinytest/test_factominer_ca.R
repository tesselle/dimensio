# Test against FactoMineR
if (at_home() && requireNamespace("FactoMineR", quietly = TRUE)) {
  library("FactoMineR")

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
  expect_equivalent(
    current = as.data.frame(res_facto$row$coord),
    target = coord_row[!coord_row$.sup, -ncol(coord_row)]
  )
  expect_equivalent(
    rownames(res_facto$row$coord),
    rownames(coord_row[!coord_row$.sup, -ncol(coord_row)])
  )

  # Supplementary row coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$row.sup$coord),
    target = coord_row[coord_row$.sup, -ncol(coord_row)]
  )
  expect_equivalent(
    rownames(res_facto$row.sup$coord),
    rownames(coord_row[coord_row$.sup, -ncol(coord_row)])
  )

  # Column principal coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$col$coord),
    target = coord_col[!coord_col$.sup, -ncol(coord_col)]
  )
  expect_equivalent(
    rownames(res_facto$col$coord),
    rownames(coord_col[!coord_col$.sup, -ncol(coord_col)])
  )

  # Supplementary column coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$col.sup$coord),
    target = coord_col[coord_col$.sup, -ncol(coord_col)]
  )
  expect_equivalent(
    rownames(res_facto$col.sup$coord),
    rownames(coord_col[coord_col$.sup, -ncol(coord_col)])
  )

  # Row contributions
  expect_equivalent(
    current = as.data.frame(res_facto$row$contrib),
    target = get_contributions(res_dim, margin = 1)
  )
  expect_equivalent(
    rownames(res_facto$row$contrib),
    rownames(get_contributions(res_dim, margin = 1))
  )

  # Column contributions
  expect_equivalent(
    current = as.data.frame(res_facto$col$contrib),
    target = get_contributions(res_dim, margin = 2)
  )
  expect_equivalent(
    rownames(res_facto$col$contrib),
    rownames(get_contributions(res_dim, margin = 2))
  )

  # Row inertias
  expect_equivalent(
    current = res_facto$row$inertia,
    target = get_inertia(res_dim, margin = 1)
  )
  expect_equivalent(
    rownames(res_facto$row$inertia),
    rownames(get_inertia(res_dim, margin = 1))
  )

  # Column inertias
  expect_equivalent(
    current = res_facto$col$inertia,
    target = get_inertia(res_dim, margin = 2)
  )
  expect_equivalent(
    rownames(res_facto$col$inertia),
    rownames(get_inertia(res_dim, margin = 2))
  )

  # Get cos2
  cos2_row <- get_cos2(res_dim, margin = 1)
  cos2_col <- get_cos2(res_dim, margin = 2)

  # Row cos2
  expect_equivalent(
    current = as.data.frame(res_facto$row$cos2),
    target = cos2_row[!cos2_row$.sup, -ncol(cos2_row)]
  )
  expect_equivalent(
    rownames(res_facto$row$cos2),
    rownames(cos2_row[!cos2_row$.sup, -ncol(cos2_row)])
  )

  # Supplementary row cos2
  expect_equivalent(
    current = as.data.frame(res_facto$row.sup$cos2),
    target = cos2_row[cos2_row$.sup, -ncol(cos2_row)]
  )
  expect_equivalent(
    rownames(res_facto$row.sup$cos2),
    rownames(cos2_row[cos2_row$.sup, -ncol(cos2_row)])
  )

  # Column cos2
  expect_equivalent(
    current = as.data.frame(res_facto$col$cos2),
    target = cos2_col[!cos2_col$.sup, -ncol(cos2_col)]
  )
  expect_equivalent(
    rownames(res_facto$col$cos2),
    rownames(cos2_col[!cos2_col$.sup, -ncol(cos2_col)])
  )

  # Supplementary column cos2
  expect_equivalent(
    current = as.data.frame(res_facto$col.sup$cos2),
    target = cos2_col[cos2_col$.sup, -ncol(cos2_col)]
  )
  expect_equivalent(
    rownames(res_facto$col.sup$cos2),
    rownames(cos2_col[cos2_col$.sup, -ncol(cos2_col)])
  )
}
