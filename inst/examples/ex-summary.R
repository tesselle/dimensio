## Load data
data("zuni", package = "folio")

## Compute correspondence analysis
X <- ca(zuni, rank = 5, sup_row = 1:50)

## Rows summary
summary(X, margin = 1)

## Columns summary
summary(X, margin = 2)
