Sys.setenv(LANGUAGE = "en") # Force locale

# PCA Biplot ===================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("countries")

  # All rows and all columns obtain the same weight
  row_w <- rep(1 / nrow(countries), nrow(countries)) # 1/13
  col_w <- rep(1 / ncol(countries), ncol(countries)) # 1/6
  X <- pca(countries, scale = FALSE, weight_row = row_w, weight_col = col_w)

  # Row-metric-preserving biplot
  biplot_row <- function() biplot(X, type = "form", labels = NULL, lwd = 2)
  expect_snapshot_plot(biplot_row, "PCA_biplot_row")

  # Column-metric-preserving biplot
  biplot_col <- function() biplot(X, type = "covariance", labels = NULL, lwd = 2)
  expect_snapshot_plot(biplot_col, "PCA_biplot_column")

  # Supplementary individuals
  X <- pca(countries, scale = FALSE, sup_row = c(1, 3, 5))
  biplot_sup_row <- function() biplot(X, type = "covariance", labels = NULL)
  expect_snapshot_plot(biplot_sup_row, "PCA_biplot_sup_row")

  # Supplementary columns
  X <- pca(countries, scale = FALSE, sup_col = c(1, 3))
  biplot_sup_col <- function() biplot(X, type = "covariance", labels = NULL)
  expect_snapshot_plot(biplot_sup_row, "PCA_biplot_sup_col")
}
