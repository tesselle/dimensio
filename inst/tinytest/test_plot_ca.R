if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("benthos")

  # CA - Plot coordinates ======================================================
  res <- ca(benthos, sup_row = 1:5, sup_col = 1)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_row <- function() viz_rows(res, axes = c(1, 2), active = i, sup = j,
                                      extra_quali = "observation", symbol = c(1, 2))
      expect_snapshot_plot(plot_row, sprintf("CA_row_%d-%d", i, j))

      plot_col <- function() viz_columns(res, axes = c(1, 2), active = i, sup = j,
                                         extra_quali = "observation", symbol = c(1, 2))
      expect_snapshot_plot(plot_col, sprintf("CA_col_%d-%d", i, j))
    }
  }

  # CA - Plot eigenvalues ======================================================
  res <- ca(benthos)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_var <- function() screeplot(res, eigenvalues = i, cumulative = j)
      expect_snapshot_plot(plot_var, sprintf("CA_eig_%d-%d", i, j))
    }
  }

  # CA - Plot contributions ====================================================
  plot_contrib_1 <- function() viz_contributions(res, margin = 1, axes = c(1, 2))
  expect_snapshot_plot(plot_contrib_1, "CA_ind_contrib")

  plot_contrib_2 <- function() viz_contributions(res, margin = 2, axes = 1)
  expect_snapshot_plot(plot_contrib_2, "CA_var_contrib")

  plot_cos2 <- function() viz_cos2(res, margin = 2)
  expect_snapshot_plot(plot_cos2, "CA_var_cos2")
}
