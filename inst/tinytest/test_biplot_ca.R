# CA Biplot ====================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("benthos")
  X <- ca(benthos)

  # Row principal CA biplot
  plot_biplot_row <- function() biplot(X, type = "row", labels = NULL)
  expect_snapshot_plot(plot_biplot_row, "CA_biplot_row")

  # Column principal CA biplot
  plot_biplot_col <- function() biplot(X, type = "column", labels = NULL)
  expect_snapshot_plot(plot_biplot_col, "CA_biplot_column")

  # Contribution CA biplot
  plot_biplot_contrib <- function() biplot(X, type = "contrib", labels = NULL)
  expect_snapshot_plot(plot_biplot_contrib, "CA_biplot_contrib")
}

