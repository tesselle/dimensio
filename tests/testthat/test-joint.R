test_that("Correspondence Analysis", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
  res <- ca(cts)

  # Coordinates
  coord <- joint(res, "coord", axes = c(1, 2))
  expect_length(coord, 20)

  # Contributions
  contrib <- joint(res, "contrib", axes = c(1, 2))
  expect_length(contrib, 20)

  # cos2
  cos2 <- joint(res, "cos2", axes = c(1, 2))
  expect_length(cos2, 20)
})
test_that("Principal Components Analysis", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
  res <- pca(cts)

  # Coordinates
  coord <- joint(res, "coord", axes = c(1, 2))
  expect_length(coord, 20)

  # Contributions
  contrib <- joint(res, "contrib", axes = c(1, 2))
  expect_length(contrib, 20)

  # cos2
  cos2 <- joint(res, "cos2", axes = c(1, 2))
  expect_length(cos2, 20)
})
