# Correspondence Analysis ======================================================
data("colours")
res <- ca(colours)

# Coordinates
coord <- dimensio:::joint(res, "coord", axes = c(1, 2))
expect_equal_to_reference(coord, file = "_snaps/ca_joint_coord.rds")

# Contributions
contrib <- dimensio:::joint(res, "contrib", axes = c(1, 2))
expect_equal_to_reference(contrib, file = "_snaps/ca_joint_contrib.rds")

# cos2
cos2 <- dimensio:::joint(res, "cos2", axes = c(1, 2))
expect_equal_to_reference(cos2, file = "_snaps/ca_joint_cos2.rds")

# Principal Components Analysis ================================================
data("iris")
res <- pca(iris)

# Coordinates
coord <- dimensio:::joint(res, "coord", axes = c(1, 2))
expect_equal_to_reference(coord, file = "_snaps/pca_joint_coord.rds")

# Contributions
contrib <- dimensio:::joint(res, "contrib", axes = c(1, 2))
expect_equal_to_reference(contrib, file = "_snaps/pca_joint_contrib.rds")

# cos2
cos2 <- dimensio:::joint(res, "cos2", axes = c(1, 2))
expect_equal_to_reference(cos2, file = "_snaps/pca_joint_cos2.rds")
