test_that("CA Biplot", {
  skip_if_not_installed("vdiffr")

  data("benthos")
  X <- ca(benthos)

  # Row principal CA biplot
  gg_biplot_row <- biplot(X, type = "row")
  vdiffr::expect_doppelganger("CA_biplot_row", gg_biplot_row)

  # Column principal CA biplot
  gg_biplot_col <- biplot(X, type = "column")
  vdiffr::expect_doppelganger("CA_biplot_column", gg_biplot_col)

  # Contribution CA biplot
  gg_biplot_contrib <- biplot(X, type = "contrib")
  vdiffr::expect_doppelganger("CA_biplot_contrib", gg_biplot_contrib)
})

