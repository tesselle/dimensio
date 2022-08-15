test_that("PCA Biplot", {
  skip_if_not_installed("vdiffr")

  data("countries")
  # All rows and all columns obtain the same weight
  row_w <- rep(1 / nrow(countries), nrow(countries)) # 1/13
  col_w <- rep(1 / ncol(countries), ncol(countries)) # 1/6
  X <- pca(countries, scale = FALSE, weight_row = row_w, weight_col = col_w)

  # Row-metric-preserving biplot
  gg_biplot_row <- biplot(X, type = "form")
  vdiffr::expect_doppelganger("PCA_biplot_row", gg_biplot_row)

  # Column-metric-preserving biplot
  gg_biplot_col <- biplot(X, type = "covariance")
  vdiffr::expect_doppelganger("PCA_biplot_column", gg_biplot_col)
})
