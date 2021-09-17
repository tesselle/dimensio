test_that("Jackknife", {
  with_seed(12345, {
    x <- rnorm(20)
    jack <- jackknife(x, do = mean)
  })

  jack1 <- summary(jack)
  expect_snapshot(jack1)
})
