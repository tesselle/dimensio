Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale

# MCA ==========================================================================
if (requireNamespace("MASS", quietly = TRUE)) {
  data(farms, package = "MASS")

  res <- mca(farms, rank = 4)
  expect_stdout(show(res), "Multiple Correspondence Analysis")
  expect_equal(rownames(res), rownames(farms))

  # Points coordinates
  expect_equal_to_reference(get_coordinates(res, margin = 1, principal = TRUE), file = "_snaps/mca_row_principal.rds")
  expect_equal_to_reference(get_coordinates(res, margin = 2, principal = TRUE), file = "_snaps/mca_col_principal.rds")
  expect_equal_to_reference(get_coordinates(res, margin = 1, principal = FALSE), file = "_snaps/mca_row_standard.rds")
  expect_equal_to_reference(get_coordinates(res, margin = 2, principal = FALSE), file = "_snaps/mca_col_standard.rds")

  # Tidy coordinates
  expect_equal_to_reference(tidy(res, margin = 1), file = "_snaps/mca_row_tidy.rds")
  expect_equal_to_reference(tidy(res, margin = 2), file = "_snaps/mca_col_tidy.rds")
  expect_equal_to_reference(augment(res, margin = 1), file = "_snaps/mca_row_augment.rds")
  expect_equal_to_reference(augment(res, margin = 2), file = "_snaps/mca_col_augment.rds")

  # Distances
  expect_equal_to_reference(get_distances(res, margin = 1), file = "_snaps/mca_row_distances.rds")
  expect_equal_to_reference(get_distances(res, margin = 2), file = "_snaps/mca_col_distances.rds")

  # Inertias
  expect_equal_to_reference(get_inertia(res, margin = 1), file = "_snaps/mca_row_inertia.rds")
  expect_equal_to_reference(get_inertia(res, margin = 2), file = "_snaps/mca_col_inertia.rds")

  # Eigenvalues
  expect_equal_to_reference(get_eigenvalues(res), file = "_snaps/mca_eigenvalues.rds")

  # MCA - data.frame =============================================================
  cts <- matrix(data = sample(LETTERS, 100, TRUE), ncol = 20)
  df <- as.data.frame(cts)
  df$test <- numeric(5)

  expect_message(mca(df, sup_col = 1:5), "quantitative variable was removed")

  # Predict new coordinates ======================================================
  res <- mca(farms)
  new_rows <- predict(res, farms, margin = 1)
  new_cols <- predict(res, farms, margin = 2)

  sup_rows <- get_coordinates(res, margin = 1)
  sup_cols <- get_coordinates(res, margin = 2)

  # FIXME: pourquoi la dernière colonne diffère ?
  expect_equivalent(new_rows[, 1:11], sup_rows[, 1:11], ignore_attr = TRUE)
  expect_equivalent(new_cols[, 1:11], sup_cols[, 1:11], ignore_attr = TRUE)
}
