## Load data
library(codex)
data("zuni")

## Compute correspondence analysis
X <- ca(zuni)

## Plot observations
plot(X)
