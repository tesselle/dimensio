## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Plot individuals
viz_individuals(X, panel.last = graphics::grid())

## Labels of the 10 individuals with highest cos2
viz_individuals(X, labels = list(how = "cos2", n = 10))

## Plot variables
viz_variables(X, panel.last = graphics::grid())

## Graphical parameters
## Continuous values
viz_individuals(X, extra_quanti = iris$Petal.Length, symbol = 16, size = c(1, 2))
viz_individuals(X, extra_quanti = iris$Petal.Length, symbol = 16, size = c(1, 2),
                color = grDevices::hcl.colors(12, "RdPu"))

viz_variables(X, extra_quanti = "contribution",
              color = grDevices::hcl.colors(12, "BluGrn", rev = TRUE),
              size = c(0, 1))

## Discrete values
viz_individuals(X, extra_quali = iris$Species, symbol = 21:23)
viz_individuals(X, extra_quali = iris$Species, symbol = 21:23,
                fill = c("#004488", "#DDAA33", "#BB5566"),
                color = "black")

viz_variables(X, extra_quali = c("Petal", "Petal", "Sepal", "Sepal"),
              color = c("#EE7733", "#0077BB"),
              symbol = c(1, 3))
