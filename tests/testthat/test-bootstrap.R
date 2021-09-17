test_that("Non-parametric boostrap", {
  with_seed(12345, {
    x <- rnorm(20)
    rboot <- bootstrap(x, do = mean, n = 30)
  })

  # With quantiles and confidence interval
  rboot1 <- summary(rboot, level = 0.95, probs = c(0.05, 0.95))
  expect_snapshot(rboot1)
})
test_that("Multinomial boostrap", {
  with_seed(12345, {
    x <- sample(1:100, 50, TRUE)
    mboot <- bootstrap(x, do = min, n = 30)
  })

  # With quantiles and confidence interval
  boot1 <- summary(mboot, level = 0.95, probs = c(0.05, 0.95))
  expect_snapshot(boot1)

  # Without confidence interval
  boot2 <- summary(mboot, level = NULL, probs = c(0.05, 0.95))
  expect_snapshot(boot2)

  # Without quantiles
  boot3 <- summary(mboot, level = 0.95, probs = NULL)
  expect_snapshot(boot3)
})
test_that("Confidence interval", {
  with_seed(12345, {
    x <- sample(1:300, 100, TRUE)
    ci <- confidence_mean(x, level = 0.95, type = "student")
    tci <- t.test(x, conf.level = 0.95)$conf.int
    expect_equal(ci, tci, ignore_attr = TRUE)
  })
})
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
