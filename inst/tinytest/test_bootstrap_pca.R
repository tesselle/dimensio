if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("iris")

  X <- pca(iris)
  Y <- with_seed(12345, bootstrap(X, n = 30))

  expect_equal_to_reference(get_coordinates(Y, margin = 2), file = "_snaps/pca_col_bootstrap.rds")
  expect_equal(dim(get_replications(Y)), c(4L, 3L, 30L))

  expect_false(dimensio:::has_groups(Y, margin = 1))
  expect_true(dimensio:::has_groups(Y, margin = 2))

  plot_boot_col <- function() {
    viz_variables(Y, axes = c(1, 2))
    viz_tolerance(Y, margin = 2, level = c(0.68, 0.95))
  }
  expect_snapshot_plot(plot_boot_col, "PCA_boot_col")
}
