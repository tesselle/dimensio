x <- data.frame(
  A = c("a", "b", "a"),
  B = c("x", "y", "z")
)

# Complete disjunctive table ===================================================
y <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 5,
            dimnames = list(NULL, c("A_a", "A_b", "B_x", "B_y", "B_z")))

expect_equal(cdt(x), y)

# Burt table ===================================================================
y <- matrix(
  c(2, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1),
  nrow = 5, ncol = 5,
  dimnames = list(c("A_a", "A_b", "B_x", "B_y", "B_z"),
                  c("A_a", "A_b", "B_x", "B_y", "B_z"))
)

expect_equal(burt(x), y)
