## Load data
data("iris")

## Compute euclidean distances
d <- dist(iris[, 1:4], method = "euclidean")

## Compute principal coordinates analysis
X <- pcoa(d)

## Screeplot
screeplot(X)

## Plot results
plot(X, extra_quali = iris$Species)
