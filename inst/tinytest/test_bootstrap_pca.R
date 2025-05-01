Sys.setenv(LANGUAGE = "en") # Force locale

if (at_home() && Sys.info()["sysname"] == "Linux") {
  using("tinysnapshot")
  source("helpers.R")

  data("iris")

  X <- pca(iris, sup_quali = 5)
  Y <- with_seed(12345, bootstrap(X, n = 30))

  expect_equal_to_reference(get_coordinates(Y, margin = 2), file = "_snaps/pca_col_bootstrap.rds")
  expect_equal(dim(get_replications(Y)), c(4L, 3L, 30L))

  expect_false(dimensio:::has_groups(Y, margin = 1))
  expect_true(dimensio:::has_groups(Y, margin = 2))

  plot_boot_col <- function() {
    viz_variables(Y, axes = c(1, 2), color = NULL, legend = list(x = "topleft"))
    viz_tolerance(Y, margin = 2, level = c(0.68, 0.95))
  }
  expect_snapshot_plot(plot_boot_col, "PCA_boot_col")
}
