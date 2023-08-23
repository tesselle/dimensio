## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Plot individuals
viz_individuals(X, panel.last = graphics::grid())

## Plot variables
viz_variables(X, panel.last = graphics::grid())

## Graphical parameters
## Continuous values
viz_individuals(X, highlight = iris$Petal.Length, pch = 16)
viz_individuals(X, highlight = iris$Petal.Length, pch = 16,
                col = grDevices::hcl.colors(12, "RdPu"))
viz_individuals(X, highlight = iris$Petal.Length, pch = 16,
                col = grDevices::hcl.colors(12, "RdPu"),
                cex = c(1, 2))

viz_variables(X, highlight = "contribution",
              col = grDevices::hcl.colors(12, "BluGrn", rev = TRUE),
              lwd = c(1, 5))

## Discrete values
viz_individuals(X, highlight = iris$Species, pch = 21:23)
viz_individuals(X, highlight = iris$Species, pch = 21:23,
                bg = c("#004488", "#DDAA33", "#BB5566"),
                col = "black")

viz_variables(X, highlight = c("Petal", "Petal", "Sepal", "Sepal"),
              col = c("#EE7733", "#0077BB"),
              lty = c(1, 3))


