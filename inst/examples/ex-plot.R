## Data from Lebart et al. 2006, p. 170-172
data("colours")

## Compute correspondence analysis
X <- ca(colours)

## Plot results
plot(X) +
  ggrepel::geom_label_repel()

## Screeplot
plot_variance(X)
