## Load data
data("iris")

## Compute principal components analysis
Y <- pca(iris, scale = TRUE)

## Plot results
plot_individuals(Y, colour = "group", shape = "group", group = iris$Species) +
  khroma::scale_colour_highcontrast()

plot_individuals(Y, colour = "group", size = "cos2", group = iris$Sepal.Width) +
  khroma::scale_color_YlOrBr()

plot_individuals(Y, colour = "contribution", size = "contribution") +
  khroma::scale_color_iridescent(range = c(0.5, 1))

plot_variables(Y, colour = "contribution") +
  ggrepel::geom_label_repel() +
  khroma::scale_color_YlOrBr(range = c(0.5, 1))
