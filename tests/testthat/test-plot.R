test_that("CA - Plot coordinates", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni, sup_row = 50:75, sup_col = 15:18)

  gg_all <- plot(res, margin = c(1, 2), axes = c(1, 2),
                 active = TRUE, sup = TRUE, highlight = NULL, group = NULL)
  vdiffr::expect_doppelganger("CA_coord-ALL", gg_all)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_ind <- plot_rows(res, axes = c(1, 2), active = i, sup = j,
                          highlight = NULL, group = NULL)
      vdiffr::expect_doppelganger(sprintf("CA_ind_%d-%d", i, j), gg_ind)

      gg_var <- plot_columns(res, axes = c(1, 2), active = i, sup = j,
                             highlight = NULL, group = NULL)
      vdiffr::expect_doppelganger(sprintf("CA_var_%d-%d", i, j), gg_var)
    }
  }
})
test_that("CA - Plot eigenvalues", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_var <- plot_variance(res, variance = i, cumulative = j)
      vdiffr::expect_doppelganger(sprintf("CA_eig_%d-%d", i, j), gg_var)
    }
  }
})
test_that("CA - Plot contributions", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni)

  gg_contrib_1 <- plot_contributions(res, margin = 1, axes = c(1, 2))
  vdiffr::expect_doppelganger("CA_ind_contrib", gg_contrib_1)

  gg_contrib_2 <- plot_contributions(res, margin = 2, axes = 1)
  vdiffr::expect_doppelganger("CA_var_contrib", gg_contrib_2)

  gg_cos2 <- plot_cos2(res, margin = 2)
  vdiffr::expect_doppelganger("CA_var_cos2", gg_cos2)
})
test_that("PCA - Plot coordinates", {
  skip_if_not_installed("vdiffr")

  data("iris")
  sup_ind <- seq(from = 1, to = 150, by = 5)
  res <- pca(iris[, -5], sup_ind = sup_ind, sup_var = 4)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_ind <- plot(res, margin = 1, axes = c(1, 2),
                     active = i, sup = j, highlight = NULL, group = NULL)
      vdiffr::expect_doppelganger(sprintf("PCA_ind_%d-%d", i, j), gg_ind)
    }
  }

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_var <- plot(res, margin = 2, axes = c(1, 2),
                     active = i, sup = j, highlight = NULL, group = NULL)
      vdiffr::expect_doppelganger(sprintf("PCA_var_%d-%d", i, j), gg_var)
    }
  }

  gg_cos2 <- plot(res, margin = 1, axes = c(1, 2), active = TRUE, sup = TRUE,
                  highlight = "cos2", group = NULL)
  vdiffr::expect_doppelganger("PCA_ind_cos2", gg_cos2)

  gg_contrib <- plot(res, margin = 2, axes = c(1, 2), active = TRUE, sup = TRUE,
                     highlight = "contrib", group = NULL)
  vdiffr::expect_doppelganger("PCA_var_contrib", gg_contrib)

  gg_group <- plot(res, margin = 1, axes = c(1, 2), active = TRUE,
                   sup = TRUE, highlight = NULL, group = iris$Species)
  vdiffr::expect_doppelganger("PCA_ind_group", gg_group)

  group_num <- seq_len(ncol(iris))
  gg_group_num <- plot(res, margin = 2, axes = c(1, 2), active = TRUE,
                       sup = TRUE, highlight = NULL, group = group_num)
  vdiffr::expect_doppelganger("PCA_var_group_num", gg_group_num)

  group_cat <- rep(c("A", "B", "C"), length.out = ncol(iris))
  gg_group_cat <- plot(res, margin = 2, axes = c(1, 2), active = TRUE,
                       sup = TRUE, highlight = NULL, group = group_cat)
  vdiffr::expect_doppelganger("PCA_var_group_cat", gg_group_cat)
})
