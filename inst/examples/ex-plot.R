## Load data
data("zuni", package = "folio")

## Compute correspondence analysis
X <- ca(zuni)

## Plot observations
plot(X)

## Screeplot
plot_variance(X)
