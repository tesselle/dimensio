if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("iris")

  # PCA - Plot coordinates =====================================================
  sup_ind <- seq(from = 1, to = 150, by = 5)
  res <- pca(iris, sup_row = sup_ind, sup_col = 4, sup_quali = 5)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_ind <- function() viz_individuals(res, axes = c(1, 2), active = i, sup = j,
                                             highlight = "observation", pch = c(1, 2))
      expect_snapshot_plot(plot_ind, sprintf("PCA_ind_%d-%d", i, j))
    }
  }

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_var <- function() viz_variables(res, axes = c(1, 2),
                                           active = i, sup = j,
                                           labels = FALSE,
                                           highlight = "observation",
                                           lty = c(1, 2))
      expect_snapshot_plot(plot_var, sprintf("PCA_var_%d-%d", i, j))
    }
  }

  # PCA - Plot aesthetics ======================================================
  res <- pca(iris, sup_quali = 5)

  # Individuals
  plot_ind_quali <- function() viz_individuals(res, highlight = "Species")
  expect_snapshot_plot(plot_ind_quali, "PCA_ind_highlight_quali")

  plot_ind_cos2 <- function() viz_individuals(res, highlight = "cos2")
  expect_snapshot_plot(plot_ind_cos2, "PCA_ind_highlight_cos2")

  plot_ind_contrib <- function() viz_individuals(res, highlight = "contrib",
                                                 cex = c(1, 2))
  expect_snapshot_plot(plot_ind_contrib, "PCA_var_highlight_contrib")

  plot_ind_group <- function() viz_individuals(res, highlight = iris$Species,
                                               pch = c(1, 2, 3))
  expect_snapshot_plot(plot_ind_group, "PCA_ind_group")

  # Variables
  group_num <- c(1, 2, 3, 4)
  plot_var_group_num <- function() viz_variables(res, labels = FALSE,
                                                 highlight = group_num,
                                                 lwd = 1)
  expect_snapshot_plot(plot_var_group_num, "PCA_var_group_num")

  group_cat <- c("Sepal", "Sepal", "Petal", "Petal")
  plot_var_group_cat <- function() viz_variables(res, labels = FALSE,
                                                 highlight = group_cat,
                                                 lty = c(1, 2))
  expect_snapshot_plot(plot_var_group_cat, "PCA_var_group_cat")

  # PCA - Plot eigenvalues =====================================================
  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_var <- function() screeplot(res, eigenvalues = i, cumulative = j)
      expect_snapshot_plot(plot_var, sprintf("PCA_eig_%d-%d", i, j))
    }
  }

  # PCA - Plot contributions ===================================================
  plot_contrib_1 <- function() viz_contributions(res, margin = 1, axes = c(1, 2))
  expect_snapshot_plot(plot_contrib_1, "PCA_ind_contrib")

  plot_contrib_2 <- function() viz_contributions(res, margin = 2, axes = 1)
  expect_snapshot_plot(plot_contrib_2, "PCA_var_contrib")

  plot_cos2 <- function() viz_cos2(res, margin = 2)
  expect_snapshot_plot(plot_cos2, "PCA_var_cos2")
}
