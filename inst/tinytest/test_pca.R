Sys.setenv(LANGUAGE = "en") # Force locale

# PCA ==========================================================================
data("countries")

row_zeros <- countries
row_zeros[1, ] <- NA
expect_error(pca(row_zeros))

res <- pca(countries, center = TRUE, scale = FALSE, rank = 5)
expect_stdout(show(res), "Principal Components Analysis")
expect_equal(dim(res), 5L)
expect_equal(rownames(res), rownames(countries))
expect_equal(colnames(res), colnames(countries))
expect_equal(dimnames(res), dimnames(countries))

expect_equal(get_data(res), countries)

# Points coordinates
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = TRUE), file = "_snaps/pca_row_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = TRUE), file = "_snaps/pca_col_principal.rds")
expect_equal_to_reference(get_coordinates(res, margin = 1, principal = FALSE), file = "_snaps/pca_row_standard.rds")
expect_equal_to_reference(get_coordinates(res, margin = 2, principal = FALSE), file = "_snaps/pca_col_standard.rds")

# Tidy coordinates
expect_equal_to_reference(tidy(res, margin = 1), file = "_snaps/pca_row_tidy.rds")
expect_equal_to_reference(tidy(res, margin = 2), file = "_snaps/pca_col_tidy.rds")
expect_equal_to_reference(augment(res, margin = 1), file = "_snaps/pca_row_augment.rds")
expect_equal_to_reference(augment(res, margin = 2), file = "_snaps/pca_col_augment.rds")

# Distances
expect_equal_to_reference(get_distances(res, margin = 1), file = "_snaps/pca_row_distances.rds")
expect_equal_to_reference(get_distances(res, margin = 2), file = "_snaps/pca_col_distances.rds")

# Inertias
expect_equal_to_reference(get_inertia(res, margin = 1), file = "_snaps/pca_row_inertia.rds")
expect_equal_to_reference(get_inertia(res, margin = 2), file = "_snaps/pca_col_inertia.rds")

# Eigenvalues
expect_equal_to_reference(get_eigenvalues(res), file = "_snaps/pca_eigenvalues.rds")

# Predict new coordinates ======================================================
cts <- matrix(data = sample(1:10, 100, TRUE), ncol = 5)
cts <- as.data.frame(cts)

res <- pca(cts, center = FALSE, scale = FALSE)
new_rows <- predict(res, cts, margin = 1)
new_cols <- predict(res, cts, margin = 2)

sup_rows <- get_coordinates(res, margin = 1)
sup_cols <- get_coordinates(res, margin = 2)

expect_equivalent(new_rows, sup_rows[, 1:4], ignore_attr = TRUE)
expect_equivalent(new_cols, sup_cols[, 1:4], ignore_attr = TRUE)
