Sys.setenv(LANGUAGE = "en") # Force locale

# Correspondence Analysis ======================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
res <- ca(cts, sup_row = 10:15)

s0 <- summary(res)
expect_inherits(s0, "MultivariateSummary")
expect_stdout(show(s0))

s1 <- summary(res, active = TRUE, sup = FALSE)
expect_inherits(s1, "MultivariateSummary")
expect_stdout(show(s1))

s2 <- summary(res, active = FALSE, sup = TRUE)
expect_inherits(s2, "MultivariateSummary")
expect_stdout(show(s2))

# Principal Components Analysis ================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
res <- pca(cts, sup_row = 10:15)

s0 <- summary(res)
expect_inherits(s0, "MultivariateSummary")
expect_stdout(show(s0))

s1 <- summary(res, active = TRUE, sup = FALSE)
expect_inherits(s1, "MultivariateSummary")
expect_stdout(show(s1))

s2 <- summary(res, active = FALSE, sup = TRUE)
expect_inherits(s2, "MultivariateSummary")
expect_stdout(show(s2))
