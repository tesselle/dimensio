test_that("Correspondence Analysis", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
  res <- ca(cts, sup_row = 10:15)

  s0 <- summary(res)
  expect_s4_class(s0, "MultivariateSummary")
  expect_output(show(s0))

  s1 <- summary(res, active = TRUE, sup = FALSE)
  expect_s4_class(s1, "MultivariateSummary")
  expect_output(show(s1))

  s2 <- summary(res, active = FALSE, sup = TRUE)
  expect_s4_class(s2, "MultivariateSummary")
  expect_output(show(s2))
})
test_that("Principal Components Analysis", {
  cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
  res <- pca(cts, sup_row = 10:15)

  s0 <- summary(res)
  expect_s4_class(s0, "MultivariateSummary")
  expect_output(show(s0))

  s1 <- summary(res, active = TRUE, sup = FALSE)
  expect_s4_class(s1, "MultivariateSummary")
  expect_output(show(s1))

  s2 <- summary(res, active = FALSE, sup = TRUE)
  expect_s4_class(s2, "MultivariateSummary")
  expect_output(show(s2))
})
