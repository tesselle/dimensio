test_that("CA - Plot coordinates", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni, sup_row = 50:75)

  gg_all <- plot(res, margin = c(1, 2), axes = c(1, 2),
                 active = TRUE, sup = TRUE, highlight = NULL, group = NULL)
  vdiffr::expect_doppelganger("CA_ind-var", gg_all)

  for (i in c(TRUE, FALSE)) {
    gg_ind <- plot(res, margin = 1, axes = c(1, 2),
                   active = TRUE, sup = i, highlight = NULL, group = NULL)
    vdiffr::expect_doppelganger(paste0("CA_ind-", i), gg_ind)
  }

  gg_var <- plot(res, margin = 2, axes = c(1, 2),
                 active = TRUE, sup = TRUE, highlight = NULL, group = NULL)
  vdiffr::expect_doppelganger("CA_var", gg_var)
})
test_that("CA - Plot eigenvalues", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni)

  gg_eig <- plot_eigenvalues(res)
  vdiffr::expect_doppelganger("CA_eig", gg_eig)

  for (i in c(TRUE, FALSE)) {
    gg_var <- plot_variance(res, variance = TRUE, cumulative = i)
    vdiffr::expect_doppelganger(paste0("CA_variance-", i), gg_var)
  }
})
test_that("CA - Plot contributions", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- ca(zuni)

  gg_contrib <- plot_contributions(res)
  vdiffr::expect_doppelganger("CA_contrib", gg_contrib)

  gg_cos2 <- plot_cos2(res, margin = 2)
  vdiffr::expect_doppelganger("CA_cos2", gg_cos2)
})
test_that("PCA - Plot coordinates", {
  skip_if_not_installed("codex")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "codex")
  res <- pca(zuni, sup_ind = 50:75, sup_var = 1:3)

  for (i in c(TRUE, FALSE)) {
    gg_ind <- plot(res, margin = 1, axes = c(1, 2),
                   active = TRUE, sup = i, highlight = NULL, group = NULL)
    vdiffr::expect_doppelganger(paste0("PCA_ind-", i), gg_ind)
  }

  gg_var <- plot(res, margin = 2, axes = c(1, 2),
                 active = TRUE, sup = TRUE, highlight = NULL, group = NULL)
  vdiffr::expect_doppelganger("PCA_var", gg_var)

  gg_cos2 <- plot(res, margin = 1, axes = c(1, 2),
                  active = TRUE, sup = TRUE, highlight = "cos2", group = NULL)
  vdiffr::expect_doppelganger("PCA_cos2", gg_cos2)
})
