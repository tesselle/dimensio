data(farms, package = "MASS")

## Compute multiple correspondence analysis
X <- mca(farms)

## Symetric MCA biplot
biplot(X, type = "symetric", labels = "columns")
