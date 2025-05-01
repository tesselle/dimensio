Sys.setenv(LANGUAGE = "en") # Force locale

# CA Biplot ====================================================================
if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("benthos")
  X <- ca(benthos)

  # Symetric CA biplot
  biplot_symetric <- function() biplot(X, type = "symetric", labels = NULL)
  expect_snapshot_plot(biplot_symetric, "CA_biplot_symetric")

  # Row principal CA biplot
  biplot_row <- function() biplot(X, type = "row", labels = NULL)
  expect_snapshot_plot(biplot_row, "CA_biplot_row")

  # Column principal CA biplot
  biplot_col <- function() biplot(X, type = "column", labels = NULL)
  expect_snapshot_plot(biplot_col, "CA_biplot_column")

  # Contribution CA biplot
  biplot_contrib <- function() biplot(X, type = "contrib", labels = NULL)
  expect_snapshot_plot(biplot_contrib, "CA_biplot_contrib")

  # Supplementary individuals
  X <- ca(benthos, sup_row = c(1, 2, 3, 4, 5))
  biplot_sup_row <- function() biplot(X, type = "symetric", labels = NULL)
  expect_snapshot_plot(biplot_sup_row, "CA_biplot_sup_row")

  # Supplementary columns
  X <- ca(benthos, sup_col = c(1, 3))
  biplot_sup_col <- function() biplot(X, type = "symetric", labels = NULL)
  expect_snapshot_plot(biplot_sup_row, "CA_biplot_sup_col")
}

