Sys.setenv(LANGUAGE = "en") # Force locale

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("iris")

  # Plot coordinates ===========================================================
  res <- pca(iris, sup_row = 1:10, sup_col = 4, sup_quali = 5)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_ind <- function() viz_individuals(res, axes = c(1, 2), active = i, sup = j,
                                             extra_quali = "observation", symbol = c(1, 2))
      expect_snapshot_plot(plot_ind, sprintf("PCA_ind_%d-%d", i, j))
    }
  }

  plot_ind_sup_extra <- function() viz_individuals(res,  extra_quali = iris$Species)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_var <- function() viz_variables(res, axes = c(1, 2),
                                           active = i, sup = j,
                                           labels = FALSE,
                                           extra_quali = "observation",
                                           symbol = c(1, 2))
      expect_snapshot_plot(plot_var, sprintf("PCA_var_%d-%d", i, j))
    }
  }

  # Aesthetics =================================================================
  res <- pca(iris, sup_quali = 5)

  ## Individuals ---------------------------------------------------------------
  plot_ind_quali <- function() viz_individuals(res, extra_quali = iris$Species,
                                               symbol = c(1, 2, 3))
  expect_snapshot_plot(plot_ind_quali, "PCA_ind_highlight_quali")

  plot_ind_cos2 <- function() viz_individuals(res, extra_quanti = "cos2", size = c(0, 3))
  expect_snapshot_plot(plot_ind_cos2, "PCA_ind_highlight_cos2")

  plot_ind_contrib <- function() viz_individuals(res, extra_quanti = "contrib", size = c(0, 3))
  expect_snapshot_plot(plot_ind_contrib, "PCA_var_highlight_contrib")

  plot_ind_group <- function() viz_individuals(res, extra_quali = iris$Species,
                                               extra_quanti = "contrib",
                                               size = c(0, 3))
  expect_snapshot_plot(plot_ind_group, "PCA_ind_highlight_quali_quanti")

  ## Variables -----------------------------------------------------------------
  group_num <- c(1, 2, 3, 4)
  plot_var_group_num <- function() viz_variables(res, labels = FALSE,
                                                 extra_quanti = group_num,
                                                 size = 1)
  expect_snapshot_plot(plot_var_group_num, "PCA_var_group_num")

  group_cat <- c("Sepal", "Sepal", "Petal", "Petal")
  plot_var_group_cat <- function() viz_variables(res, labels = FALSE,
                                                 extra_quali = group_cat,
                                                 symbol = c(1, 2))
  expect_snapshot_plot(plot_var_group_cat, "PCA_var_group_cat")

  # Envelopes ==================================================================
  plot_ind_hull <- function() viz_individuals(res, extra_quali = iris$Species,
                                              hull = TRUE)
  expect_snapshot_plot(plot_ind_hull, "PCA_ind_hull")

  ellipse <- list(type = "confidence", level = 0.95)
  plot_ind_conf <- function() viz_individuals(res, extra_quali = iris$Species,
                                              ellipse = ellipse)
  expect_snapshot_plot(plot_ind_hull, "PCA_ind_confidence")

  ellipse <- list(type = "tolerance", level = 0.95)
  plot_ind_tol <- function() viz_individuals(res, extra_quali = iris$Species,
                                              ellipse = ellipse)
  expect_snapshot_plot(plot_ind_hull, "PCA_ind_tolerance")

  # Eigenvalues ================================================================
  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      plot_var <- function() screeplot(res, eigenvalues = i, cumulative = j)
      expect_snapshot_plot(plot_var, sprintf("PCA_eig_%d-%d", i, j))
    }
  }

  # Contributions ==============================================================
  plot_contrib_1 <- function() viz_contributions(res, margin = 1, axes = c(1, 2))
  expect_snapshot_plot(plot_contrib_1, "PCA_ind_contrib")

  plot_contrib_2 <- function() viz_contributions(res, margin = 2, axes = 1)
  expect_snapshot_plot(plot_contrib_2, "PCA_var_contrib")

  # Squared cosine =============================================================
  plot_cos2 <- function() viz_cos2(res, margin = 2)
  expect_snapshot_plot(plot_cos2, "PCA_var_cos2")
}
