Sys.setenv(LANGUAGE = "en") # Force locale

# Complete disjunctive table ===================================================
x <- data.frame(
  A = c("a", "b", "a"),
  B = c("x", "y", "z"),
  row.names = c("XX", "YY", "ZZ")
)
y <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 5,
            dimnames = list(c("XX", "YY", "ZZ"), c("a", "b", "x", "y", "z")))
expect_equal(cdt(x, abbrev = TRUE), y)

x[2, 1] <- NA
y <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 5,
               dimnames = list(c("XX", "YY", "ZZ"), c("A_a", "A_NA", "B_x", "B_y", "B_z")))
expect_equal(cdt(x, abbrev = FALSE, exclude = NULL), y)

y <- matrix(c(1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 4,
            dimnames = list(c("XX", "YY", "ZZ"), c("A_a", "B_x", "B_y", "B_z")))
expect_equal(cdt(x, abbrev = FALSE, exclude = NA), y)


# Burt table ===================================================================
x <- data.frame(
  A = c("a", "b", "a"),
  B = c("x", "y", "z")
)
y <- matrix(
  c(2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1),
  nrow = 5, ncol = 5,
  dimnames = list(c("A_a", "A_b", "B_x", "B_y", "B_z"),
                  c("A_a", "A_b", "B_x", "B_y", "B_z"))
)

expect_equal(burt(x, abbrev = FALSE), y)
