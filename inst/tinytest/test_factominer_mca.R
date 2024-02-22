# Test against FactoMineR
if (at_home() && requireNamespace("FactoMineR", quietly = TRUE)) {
  data(tea, package = "FactoMineR")

  res_facto <- FactoMineR::MCA(tea, ncp = 5, ind.sup = 1:3, quanti.sup = 19,
                               quali.sup = 20:36, graph = FALSE)
  res_dim <- mca(tea, rank = 5, sup_row = 1:3, sup_col = 20:36, sup_quanti = 19)

  arrange1 <- function(x) { x[order(x[, 1]), ] }

  # Get coordinates
  coord_row <- get_coordinates(res_dim, margin = 1)
  coord_col <- get_coordinates(res_dim, margin = 2)

  # Row principal coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$ind$coord),
    target = coord_row[!coord_row$.sup, -ncol(coord_row)]
  )
  expect_equivalent(
    rownames(res_facto$ind$coord),
    rownames(coord_row[!coord_row$.sup, -ncol(coord_row)])
  )

  # Supplementary row coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$ind.sup$coord),
    target = coord_row[coord_row$.sup, -ncol(coord_row)]
  )
  expect_equivalent(
    rownames(res_facto$ind.sup$coord),
    rownames(coord_row[coord_row$.sup, -ncol(coord_row)])
  )

  # Column principal coordinates
  expect_equivalent(
    current = as.data.frame(res_facto$var$coord),
    target = coord_col[!coord_col$.sup, -ncol(coord_col)]
  )
  expect_equivalent(
    rownames(res_facto$var$coord),
    rownames(coord_col[!coord_col$.sup, -ncol(coord_col)])
  )

  # Supplementary column coordinates
  expect_equivalent(
    current = arrange1(as.data.frame(res_facto$quali.sup$coord)),
    target = arrange1(coord_col[coord_col$.sup, -ncol(coord_col)])
  )

  # Row contributions
  expect_equivalent(
    current = as.data.frame(res_facto$ind$contrib),
    target = get_contributions(res_dim, margin = 1)
  )
  expect_equivalent(
    rownames(res_facto$ind$contrib),
    rownames(get_contributions(res_dim, margin = 1))
  )

  # Column contributions
  expect_equivalent(
    current = as.data.frame(res_facto$var$contrib),
    target = get_contributions(res_dim, margin = 2)
  )

  # Get cos2
  cos2_row <- get_cos2(res_dim, margin = 1)
  cos2_col <- get_cos2(res_dim, margin = 2)

  # Row cos2
  expect_equivalent(
    current = as.data.frame(res_facto$ind$cos2),
    target = cos2_row[!cos2_row$.sup, -ncol(cos2_row)]
  )
  expect_equivalent(
    rownames(res_facto$ind$cos2),
    rownames(cos2_row[!cos2_row$.sup, -ncol(cos2_row)])
  )

  # Supplementary row cos2
  expect_equivalent(
    current = as.data.frame(res_facto$ind.sup$cos2),
    target = cos2_row[cos2_row$.sup, -ncol(cos2_row)]
  )
  expect_equivalent(
    rownames(res_facto$ind.sup$cos2),
    rownames(cos2_row[cos2_row$.sup, -ncol(cos2_row)])
  )

  # Column cos2
  expect_equivalent(
    current = as.data.frame(res_facto$var$cos2),
    target = cos2_col[!cos2_col$.sup, -ncol(cos2_col)]
  )

  # Supplementary column cos2
  expect_equivalent(
    current = arrange1(as.data.frame(res_facto$quali.sup$cos2)),
    target = arrange1(cos2_col[cos2_col$.sup, -ncol(cos2_col)])
  )
}
