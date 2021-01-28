
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dimensio <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/dimensio/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/dimensio/actions)
[![codecov](https://codecov.io/gh/tesselle/dimensio/branch/master/graph/badge.svg)](https://codecov.io/gh/tesselle/dimensio)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

Simple Principal Components Analysis (PCA) and Correspondence Analysis
(CA) based on the Singular Value Decomposition (SVD). This package
provides S4 classes and methods to compute, extract, summarize and
visualize results of multivariate data analysis.

There are many very good packages for multivariate data analysis (such
as [**FactoMineR**](http://factominer.free.fr/),
[**ade4**](https://pbil.univ-lyon1.fr/ade4/) or
[**ca**](https://cran.r-project.org/package=ca), all extended by
[**FactoExtra**](https://rpkgs.datanovia.com/factoextra)). **dimensio**
is designed to be as simple as possible, providing all the necessary
tools to explore the results of the analysis.

## Installation

You can install the released version of **dimensio** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dimensio")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/dimensio")
```

## Usage

``` r
## Load packages
library(dimensio)

library(ggplot2)
library(ggrepel)
library(khroma)
```

``` r
## Select 25 random supplementary individuals
set.seed(12345)
sup <- sample(nrow(iris), size = 25, replace = TRUE)

## Compute PCA
## (non numeric variables are automatically removed)
X <- pca(iris, scale = TRUE, sup_ind = sup)
#> 1 qualitative variable was removed: Species.
```

### Summarize

``` r
## Summarize results for the individuals (first two components)
summary(X, margin = 1, rank = 2)
#> --- Principal Components Analysis (PCA) -----------------------------------------
#> 
#> Eigenvalues:
#>     eigenvalues variance cumulative
#> PC1       2.923   73.461     73.461
#> PC2       0.898   22.575     96.037
#> PC3       0.158    3.963    100.000
#> 
#> Active individuals:
#>     dist PC1_coord PC1_contrib PC1_cos2 PC2_coord PC2_contrib PC2_cos2
#> 4  2.478    -2.380       1.526    0.922    -0.682       0.408    0.076
#> 5  2.532    -2.477       1.652    0.956     0.526       0.243    0.043
#> 6  2.552    -2.162       1.259    0.717     1.356       1.612    0.282
#> 7  2.553    -2.529       1.723    0.981    -0.071       0.004    0.001
#> 8  2.321    -2.316       1.445    0.995     0.125       0.014    0.003
#> 9  2.693    -2.413       1.568    0.803    -1.188       1.238    0.195
#> 11 2.452    -2.252       1.366    0.843     0.933       0.762    0.145
#> 13 2.442    -2.297       1.421    0.885    -0.792       0.550    0.105
#> 15 2.915    -2.287       1.409    0.615     1.735       2.639    0.354
#> 16 3.445    -2.356       1.495    0.468     2.512       5.532    0.532
#> (127 more)
#> 
#> Supplementary individuals:
#>     dist PC1_coord PC1_cos2 PC2_coord PC2_cos2
#> 1  2.382    -2.349    0.972     0.376    0.025
#> 2  2.293    -2.157    0.885    -0.733    0.102
#> 3  2.484    -2.446    0.969    -0.433    0.030
#> 10 2.343    -2.264    0.934    -0.538    0.053
#> 12 2.417    -2.410    0.995     0.025    0.000
#> 14 2.918    -2.716    0.866    -1.052    0.130
#> 30 2.389    -2.347    0.965    -0.428    0.032
#> 38 2.662    -2.616    0.966     0.467    0.031
#> 40 2.266    -2.252    0.988     0.176    0.006
#> 51 1.553     1.061    0.467     0.898    0.335
#> (23 more)
```

### Extract

**dimesion** provides several methods to extract the results:

-   `get_data()` returns the original data.
-   `get_contributions()` returns the contributions to the definition of
    the principal dimensions.
-   `get_coordinates()` returns the principal coordinates.
-   `get_correlations()` returns the correlations between variables and
    dimensions.
-   `get_cos2()` returns the cos<sup>2</sup> values (i.e. the quality of
    the representation of the points on the factor map).
-   `get_eigenvalues()` returns the eigenvalues, the percentages of
    variance and the cumulative percentages of variance.

``` r
## Eigenvalues
get_eigenvalues(X)
#>     eigenvalues  variance cumulative
#> PC1   2.9228862 73.461363   73.46136
#> PC2   0.8982303 22.575364   96.03673
#> PC3   0.1576910  3.963273  100.00000
```

### Visualize

**dimensio** uses [**ggplot2**](https://github.com/tidyverse/ggplot2)
for plotting informations. Visualization methods produce graphics with
as few elements as possible: this makes it easy to customize diagrams
(e.g. using extra layers, themes and scales).

``` r
## Plot active individuals by group
plot_individuals(X, group = iris$Species, active = TRUE, sup = FALSE) +
  ggplot2::stat_ellipse() + # Add ellipses
  ggplot2::theme_bw() + # Change theme
  khroma::scale_color_contrast() # Custom color scale

## Plot all individuals by cos2
plot_individuals(X, highlight = "cos2", active = TRUE, sup = TRUE) +
  ggplot2::theme_bw() + # Change theme
  khroma::scale_color_iridescent() # Custom color scale
```

![](man/figures/README-plot-ind-1.png)![](man/figures/README-plot-ind-2.png)

``` r
## Plot variables factor map
plot_variables(X) +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme(legend.position = "bottom") # Edit theme

## Highlight contributions
plot_variables(X, highlight = "contrib") +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme(legend.position = "bottom") + # Edit theme
  khroma::scale_color_YlOrBr(range = c(0.5, 1)) # Custom color scale
```

![](man/figures/README-plot-var-1.png)![](man/figures/README-plot-var-2.png)

``` r
## Plot eigenvalues
plot_eigenvalues(X) +
  ggplot2::theme_bw() # Change theme

## Plot percentages of variance
plot_variance(X, cumulative = TRUE) +
  ggplot2::geom_text(nudge_y = 3) + # Add labels
  ggplot2::theme_bw() # Change theme

## Plot variables contributions to the definition of the first component
plot_contributions(X, margin = 2, axes = 1) +
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme( # Edit theme
    # Rotate x axis labels
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
```

![](man/figures/README-plot-eig-1.png)![](man/figures/README-plot-eig-2.png)![](man/figures/README-plot-eig-3.png)

## Contributing

Please note that the **dimensio** project is released with a
[Contributor Code of
Conduct](https://github.com/tesselle/dimensio/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
