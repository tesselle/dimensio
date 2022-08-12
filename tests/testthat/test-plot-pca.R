test_that("PCA - Plot coordinates", {
  skip_if_not_installed("vdiffr")

  data("iris")
  sup_ind <- seq(from = 1, to = 150, by = 5)
  res <- pca(iris, sup_row = sup_ind, sup_col = 4)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_ind <- plot_individuals(res, axes = c(1, 2), active = i, sup = j)
      vdiffr::expect_doppelganger(sprintf("PCA_ind_%d-%d", i, j), gg_ind)
    }
  }

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_var <- plot_variables(res, axes = c(1, 2), active = i, sup = j)
      vdiffr::expect_doppelganger(sprintf("PCA_var_%d-%d", i, j), gg_var)
    }
  }
})
test_that("PCA - Plot aesthetics", {
  skip_if_not_installed("vdiffr")

  data("iris")
  res <- pca(iris)

  # Individuals
  gg_cos2 <- plot_individuals(res, colour = "cos2", alpha = "cos2")
  vdiffr::expect_doppelganger("PCA_ind_highlight_cos2", gg_cos2)

  gg_contrib <- plot_individuals(res, size = "contrib")
  vdiffr::expect_doppelganger("PCA_var_highlight_contrib", gg_contrib)

  gg_group <- plot_individuals(res, colour = "group", shape = "group",
                               group = iris$Species)
  vdiffr::expect_doppelganger("PCA_ind_group", gg_group)

  # Variables
  group_num <- c(1, 2, 3, 4)
  gg_group_num <- plot_variables(res, colour = "group", group = group_num)
  vdiffr::expect_doppelganger("PCA_var_group_num", gg_group_num)

  group_cat <- c("Sepal", "Sepal", "Petal", "Petal")
  gg_group_cat <- plot_variables(res, linetype = "group", group = group_cat)
  vdiffr::expect_doppelganger("PCA_var_group_cat", gg_group_cat)
})
test_that("PCA - Plot eigenvalues", {
  skip_if_not_installed("vdiffr")

  data("iris")
  res <- pca(iris)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_var <- plot_variance(res, variance = i, cumulative = j)
      vdiffr::expect_doppelganger(sprintf("PCA_eig_%d-%d", i, j), gg_var)
    }
  }
})
test_that("PCA - Plot contributions", {
  skip_if_not_installed("vdiffr")

  data("iris")
  res <- pca(iris)

  gg_contrib_1 <- plot_contributions(res, margin = 1, axes = c(1, 2))
  vdiffr::expect_doppelganger("PCA_ind_contrib", gg_contrib_1)

  gg_contrib_2 <- plot_contributions(res, margin = 2, axes = 1)
  vdiffr::expect_doppelganger("PCA_var_contrib", gg_contrib_2)

  gg_cos2 <- plot_cos2(res, margin = 2)
  vdiffr::expect_doppelganger("PCA_var_cos2", gg_cos2)
})
