# # Benchmarking =================================================================
# mtx <- matrix(data = sample(1:10, 10000, TRUE), ncol = 10)
# dtf <- as.data.frame(mtx)
#
# mat <- matrix(rnorm(1500000), ncol = 100)
# v <- rnorm(100)
# v2 <- matrix(v, nrow = nrow(mat), ncol = 100, byrow = TRUE)
#
# microbenchmark::microbenchmark(
#   a = mat %*% diag(v),
#   b = sweep(mat, 2, v, FUN = "*"),
#   c = t(t(mat) * v),
#   d = mat * v2
# )
#
# ## CA (no supplementary points)
# mbm <- microbenchmark::microbenchmark(
#   dimensio_ca = dimensio::ca(mtx, rank = 5),
#   ca_ca = ca::ca(mtx, nd = 5),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, graph = FALSE),
#   times = 5000
# )
# mbm
#
# ## CA (supplementary points)
# mbm <- microbenchmark::microbenchmark(
#   dimensio_ca = dimensio::ca(mtx, rank = 5, sup_row = 8:10, sup_col = 7:10),
#   ca_ca = ca::ca(mtx, nd = 5, suprow = 8:10, supcol = 7:10),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, row.sup = 8:10, col.sup = 7:10, graph = FALSE),
#   times = 1000
# )
# mbm
#
# ## PCA (no supplementary points)
# mbm <- microbenchmark::microbenchmark(
#   dimensio_pca = dimensio::pca(dtf, rank = 5),
#   facto_pca = FactoMineR::PCA(dtf, ncp = 5, graph = FALSE),
#   times = 5000
# )
# mbm
#
# ## PCA (supplementary points)
# mbm <- microbenchmark::microbenchmark(
#   dimensio_pca = dimensio::pca(dtf, rank = 5, sup_row = 8:10, sup_col = 7:10),
#   facto_pca = FactoMineR::PCA(dtf, ncp = 5, ind.sup = 8:10, quanti.sup = 7:10, graph = FALSE),
#   times = 1000
# )
# mbm
#
# # Profiling ====================================================================
# mtx2 <- matrix(data = sample(1:100, 1000000, TRUE), ncol = 100)
#
# ## CA
# profvis::profvis(
#   dimensio::ca(mtx2, rank = NULL, sup_row = NULL, sup_col = NULL)
# )
#
# ## PCA
# profvis::profvis(
#   dimensio::pca(mtx2, rank = 5, sup_ind = NULL, sup_var = NULL)
# )
