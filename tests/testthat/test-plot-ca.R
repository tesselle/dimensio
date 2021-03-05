test_that("CA - Plot coordinates", {
  skip_if_not_installed("folio")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "folio")
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
  skip_if_not_installed("folio")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "folio")
  res <- ca(zuni)

  for (i in c(TRUE, FALSE)) {
    for (j in c(TRUE, FALSE)) {
      gg_var <- plot_variance(res, variance = i, cumulative = j)
      vdiffr::expect_doppelganger(sprintf("CA_eig_%d-%d", i, j), gg_var)
    }
  }
})
test_that("CA - Plot contributions", {
  skip_if_not_installed("folio")
  skip_if_not_installed("vdiffr")

  data("zuni", package = "folio")
  res <- ca(zuni)

  gg_contrib_1 <- plot_contributions(res, margin = 1, axes = c(1, 2))
  vdiffr::expect_doppelganger("CA_ind_contrib", gg_contrib_1)

  gg_contrib_2 <- plot_contributions(res, margin = 2, axes = 1)
  vdiffr::expect_doppelganger("CA_var_contrib", gg_contrib_2)

  gg_cos2 <- plot_cos2(res, margin = 2)
  vdiffr::expect_doppelganger("CA_var_cos2", gg_cos2)
})
