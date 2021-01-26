## Load data
library(codex)
data("mississippi")

## Compute correspondence analysis
X <- ca(mississippi, sup_row = 11:15)

## Plot observations
plot_coordinates(X)
