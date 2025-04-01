Sys.setenv(LANGUAGE = "en") # Force locale

# PCoA =========================================================================
data("iris")

d <- dist(iris[, 1:4], method = "euclidean")
X <- pcoa(d)

expect_equal_to_reference(get_coordinates(X), file = "_snaps/pcoa_points.rds")

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_pcoa_quali <- function() plot(X, extra_quali = iris$Species)
  expect_snapshot_plot(plot_pcoa_quali, "PCOA_quali")

  plot_pcoa_quanti <- function() plot(X, extra_quanti = iris$Sepal.Length)
  expect_snapshot_plot(plot_pcoa_quanti, "PCOA_quanti")

  plot_pcoa_hull <- function() plot(X, extra_quali = iris$Species, hull = TRUE)
  expect_snapshot_plot(plot_pcoa_hull, "PCOA_hull")

  ellipse <- list(type = "confidence", level = 0.95)
  plot_pcoa_conf <- function() plot(X, extra_quali = iris$Species, ellipse = ellipse)
  expect_snapshot_plot(plot_pcoa_conf, "PCOA_confidence")

  ellipse <- list(type = "tolerance", level = 0.95)
  plot_pcoa_tol <- function() plot(X, extra_quali = iris$Species, ellipse = ellipse)
  expect_snapshot_plot(plot_pcoa_tol, "PCOA_tolerance")

  screeplot_pcoa <- function() screeplot(X)
  expect_snapshot_plot(screeplot_pcoa, "PCOA_eig")
}
