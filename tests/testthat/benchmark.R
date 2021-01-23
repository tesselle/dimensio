# ## Microbenchmarking
# mtx <- matrix(data = sample(1:10, 1000, TRUE), ncol = 10)
# dtf <- as.data.frame(mtx)
#
# ## No supplementary points
# mbm <- microbenchmark::microbenchmark(
#   arkhe_ca = dimensio::ca(mtx, n = 5),
#   ca_ca = ca::ca(mtx, nd = 5),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, graph = FALSE),
#   times = 1000
# )
# mbm
#
# ## Supplementary points
# mbm <- microbenchmark::microbenchmark(
#   arkhe_ca = dimensio::ca(mtx, n = 5, sup_row = 8:10, sup_col = 7:10),
#   ca_ca = ca::ca(mtx, nd = 5, suprow = 8:10, supcol = 7:10),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, row.sup = 8:10, col.sup = 7:10, graph = FALSE),
#   times = 1000
# )
# mbm
#
# ## data.frame
# mbm <- microbenchmark::microbenchmark(
#   arkhe_ca = dimensio::pca(dtf, n = 5, sup_ind = 8:10, sup_var = 7:10),
#   facto_ca = FactoMineR::PCA(dtf, ncp = 5, ind.sup = 8:10, quanti.sup = 7:10, graph = FALSE),
#   times = 1000
# )
# mbm
#
# ## Profiling
# mtx <- matrix(data = sample(1:100, 100000, TRUE), ncol = 100)
# profvis::profvis(
#   dimensio::ca(mtx, n = NULL, sup_row = NULL, sup_col = NULL)
# )
