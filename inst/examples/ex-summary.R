## Data from Lebart et al. 2006, p. 170-172
data("colours")

## Compute correspondence analysis
X <- ca(colours)

## Rows summary
summary(X, margin = 1)

## Columns summary
summary(X, margin = 2)
