data("iris")

options("dimensio.verbose" = TRUE)
expect_message(dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE))
options("dimensio.verbose" = FALSE)

expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE),
  list(data = as.matrix(iris[, 1:4]), sup = NULL, extra = NULL)
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE, sup = 1),
  list(data = as.matrix(iris[, c(2, 3, 4, 1)]), sup = 4L, extra = NULL)
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE, sup = 5),
  list(data = as.matrix(iris[, 1:4]), sup = NULL, extra = NULL)
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE, extra = 5),
  list(data = as.matrix(iris[, 1:4]), sup = NULL, extra = iris[, 5, drop = FALSE])
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = TRUE, sup = 1, extra = 5),
  list(data = as.matrix(iris[, c(2, 3, 4, 1)]), sup = 4L, extra = iris[, 5, drop = FALSE])
)
expect_error(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = FALSE)
)
expect_error(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = FALSE, sup = 5)
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = FALSE, extra = 5),
  list(data = as.matrix(iris[, 1:4]), sup = NULL, extra = iris[, 5, drop = FALSE])
)
expect_equal(
  dimensio:::drop_variable(iris, f = is.numeric, negate = TRUE, auto = FALSE, sup = 1, extra = 5),
  list(data = as.matrix(iris[, c(2, 3, 4, 1)]), sup = 4L, extra = iris[, 5, drop = FALSE])
)
