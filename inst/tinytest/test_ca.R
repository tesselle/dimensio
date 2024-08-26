# CA ===========================================================================
data("benthos")

row_zeros <- col_zeros <- benthos
row_zeros[1, ] <- 0
expect_error(ca(row_zeros), "Empty rows detected.")
col_zeros[, 1] <- 0
expect_error(ca(col_zeros), "Empty columns detected.")

res <- ca(benthos, rank = 10)
expect_stdout(show(res), "Correspondence Analysis")
expect_equal(rownames(res), rownames(benthos))
expect_equal(colnames(res), colnames(benthos))
expect_equal(dimnames(res), dimnames(benthos))

expect_equal(get_data(res), benthos)

# Points coordinates
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = TRUE), file = "_snaps/ca_row_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = TRUE), file = "_snaps/ca_col_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = FALSE), file = "_snaps/ca_row_standard.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = FALSE), file = "_snaps/ca_col_standard.rds")

# Tidy coordinates
expect_equal_to_reference(tidy(res, margin = 1), file = "_snaps/ca_row_tidy.rds")
expect_equal_to_reference(tidy(res, margin = 2), file = "_snaps/ca_col_tidy.rds")
expect_equal_to_reference(augment(res, margin = 1), file = "_snaps/ca_row_augment.rds")
expect_equal_to_reference(augment(res, margin = 2), file = "_snaps/ca_col_augment.rds")

# Distances
expect_equal_to_reference(get_distances(res, margin = 1), file = "_snaps/ca_row_distances.rds")
expect_equal_to_reference(get_distances(res, margin = 2), file = "_snaps/ca_col_distances.rds")

# Inertias
chi2 <- suppressWarnings(stats::chisq.test(benthos)$statistic / sum(benthos))
expect_equivalent(sum(get_inertia(res)), chi2)

expect_equal_to_reference(get_inertia(res, margin = 1), file = "_snaps/ca_row_inertia.rds")
expect_equal_to_reference(get_inertia(res, margin = 2), file = "_snaps/ca_col_inertia.rds")

# Eigenvalues
expect_equal_to_reference(get_eigenvalues(res), file = "_snaps/ca_eigenvalues.rds")

# CA - data.frame ==============================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 20)
df <- as.data.frame(cts)
df$test <- character(5)

expect_message(ca(df, sup_col = 1:5), "qualitative variable was removed")

# Predict new coordinates ======================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)

res <- ca(cts)
new_rows <- predict(res, cts, margin = 1)
new_cols <- predict(res, cts, margin = 2)

sup_rows <- get_coordinates(res, margin = 1)
sup_cols <- get_coordinates(res, margin = 2)

expect_equivalent(new_rows, sup_rows[, 1:4], ignore_attr = TRUE)
expect_equivalent(new_cols, sup_cols[, 1:4], ignore_attr = TRUE)
