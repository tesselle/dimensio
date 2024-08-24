# PCoA =========================================================================
data("iris")

d <- dist(iris[, 1:4], method = "euclidean")
res <- pcoa(d)

expect_equal_to_reference(get_coordinates(res), file = "_snaps/pcoa_points.rds")
