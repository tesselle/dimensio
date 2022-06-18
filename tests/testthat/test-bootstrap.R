test_that("CA", {
  color <- data.frame(
    brun = c(68, 15, 5, 20),
    chatain = c(119, 54, 29, 84),
    roux = c(26, 14, 14, 17),
    blond = c(7, 10, 16, 94),
    row.names = c("marron", "noisette", "vert", "bleu")
  )
  X <- ca(color)
  Y <- with_seed(12345, bootstrap(X, n = 30))
  expect_snapshot(get_coordinates(Y, margin = 1), cran = FALSE, error = FALSE)
  expect_equal(dim(get_replications(Y, margin = 1)), c(4L, 3L, 30L))

  expect_snapshot(get_coordinates(Y, margin = 2), cran = FALSE, error = FALSE)
  expect_equal(dim(get_replications(Y, margin = 2)), c(4L, 3L, 30L))

  expect_true(has_groups(Y, margin = 1))
  expect_true(has_groups(Y, margin = 2))
})
test_that("PCA", {
  data(iris)
  X <- suppressMessages(pca(iris))
  Y <- with_seed(12345, bootstrap(X, n = 30))
  expect_snapshot(get_coordinates(Y, margin = 2), cran = FALSE, error = FALSE)
  expect_equal(dim(get_replications(Y)), c(4L, 3L, 30L))

  expect_false(has_groups(Y, margin = 1))
  expect_true(has_groups(Y, margin = 2))
})
