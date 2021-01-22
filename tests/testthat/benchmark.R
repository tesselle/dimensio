# mtx <- matrix(data = sample(1:10, 100, TRUE), ncol = 10)
# dtf <- as.data.frame(mtx)

## No supplementary points
# mbm <- microbenchmark::microbenchmark(
#   arkhe_ca = dimensio::ca(mtx, n = 5),
#   ca_ca = ca::ca(mtx, nd = 5),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, graph = FALSE),
#   times = 1000
# )
# mbm

## Supplementary points
# mbm <- microbenchmark::microbenchmark(
#   arkhe_ca = dimensio::ca(mtx, n = 5, sup_rows = 8:10, sup_columns = 7:10),
#   ca_ca = ca::ca(mtx, nd = 5, suprow = 8:10, supcol = 7:10),
#   facto_ca = FactoMineR::CA(dtf, ncp = 5, row.sup = 8:10, col.sup = 7:10, graph = FALSE),
#   times = 1000
# )
# mbm
