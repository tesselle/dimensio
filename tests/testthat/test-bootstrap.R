test_that("CA", {
  data("colours")

  X <- ca(colours)
  Y <- with_seed(12345, bootstrap(X, n = 30))

  expect_snapshot(get_coordinates(Y, margin = 1))
  expect_equal(dim(get_replications(Y, margin = 1)), c(4L, 3L, 30L))

  expect_snapshot(get_coordinates(Y, margin = 2))
  expect_equal(dim(get_replications(Y, margin = 2)), c(4L, 3L, 30L))

  expect_true(has_groups(Y, margin = 1))
  expect_true(has_groups(Y, margin = 2))

  expect_snapshot(wrap_hull(Y))

  gg_boot_row <- plot_rows(Y, axes = c(1, 2), colour = "group") +
    stat_hull(geom = "path")
  vdiffr::expect_doppelganger("CA_boot_row", gg_boot_row)

  gg_boot_col <- plot_columns(Y, axes = c(1, 2), colour = "group") +
    stat_hull(geom = "path")
  vdiffr::expect_doppelganger("CA_boot_col", gg_boot_col)
})
test_that("PCA", {
  data("iris")

  X <- pca(iris)
  Y <- with_seed(12345, bootstrap(X, n = 30))
  expect_snapshot(get_coordinates(Y, margin = 2))
  expect_equal(dim(get_replications(Y)), c(4L, 3L, 30L))

  expect_false(has_groups(Y, margin = 1))
  expect_true(has_groups(Y, margin = 2))

  expect_snapshot(wrap_hull(Y))

  gg_boot_var <- plot_columns(Y, axes = c(1, 2), colour = "group") +
    stat_hull(geom = "path")
  vdiffr::expect_doppelganger("PCA_boot_var", gg_boot_var)
})
