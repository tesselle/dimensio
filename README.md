
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dimensio <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/dimensio/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/dimensio/actions)
[![codecov](https://codecov.io/gh/tesselle/dimensio/branch/main/graph/badge.svg?token=0mcb7gbZu3)](https://app.codecov.io/gh/tesselle/dimensio)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/dimensio/badge/main)](https://www.codefactor.io/repository/github/tesselle/dimensio/overview/main)
[![Dependencies](https://tinyverse.netlify.app/badge/dimensio)](https://cran.r-project.org/package=dimensio)

<a href="https://tesselle.r-universe.dev/dimensio"
class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/dimensio"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=dimensio"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/dimensio"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_dimensio.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/dimensio.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=dimensio"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/dimensio"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4478530.svg)](https://doi.org/10.5281/zenodo.4478530)
<!-- badges: end -->

## Overview

Simple Principal Components Analysis (PCA; see `vignette("pca")`) and
(Multiple) Correspondence Analysis (CA) based on the Singular Value
Decomposition (SVD). This package provides S4 classes and methods to
compute, extract, summarize and visualize results of multivariate data
analysis. It also includes methods for partial bootstrap validation.

There are many very good packages for multivariate data analysis (such
as [**FactoMineR**](http://factominer.free.fr/),
[**ade4**](https://pbil.univ-lyon1.fr/ade4/),
[**vegan**](https://rpubs.com/brouwern/veganpca) or
[**ca**](https://cran.r-project.org/package=ca), all extended by
[**FactoExtra**](https://rpkgs.datanovia.com/factoextra/)). **dimensio**
is designed to be as simple as possible, providing all the necessary
tools to explore the results of the analysis.

------------------------------------------------------------------------

To cite dimensio in publications use:

Frerebeau N (2024). *dimensio: Multivariate Data Analysis*. Université
Bordeaux Montaigne, Pessac, France. <doi:10.5281/zenodo.4478530>
<https://doi.org/10.5281/zenodo.4478530>, R package version 0.8.1,
<https://packages.tesselle.org/dimensio/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

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
## Load package
library(dimensio)
```

### Compute

``` r
## Load data
data(iris)

## Compute PCA
X <- pca(iris, center = TRUE, scale = TRUE, sup_quali = "Species")
```

### Extract

**dimensio** provides several methods to extract the results:

- `get_data()` returns the original data.
- `get_contributions()` returns the contributions to the definition of
  the principal dimensions.
- `get_coordinates()` returns the principal or standard coordinates.
- `get_correlations()` returns the correlations between variables and
  dimensions.
- `get_cos2()` returns the cos<sup>2</sup> values (i.e. the quality of
  the representation of the points on the factor map).
- `get_eigenvalues()` returns the eigenvalues, the percentages of
  variance and the cumulative percentages of variance.

### Visualize

The package allows to quickly visualize the results:

- `biplot()` produces a biplot.
- `screeplot()` produces a scree plot.
- `viz_rows()`/`viz_individuals()` displays row/individual principal
  coordinates.
- `viz_columns()`/`viz_variables()` displays columns/variable principal
  coordinates. `viz_variables()` depicts the variables by rays emanating
  from the origin (both their lengths and directions are important to
  the interpretation).
- `viz_contributions()` displays (joint) contributions.
- `viz_cos2()` displays (joint) cos<sup>2</sup>.

The `viz_*()` functions allow to highlight additional information by
varying different graphical elements (color, transparency, shape and
size of symbols…).

``` r
## Form biplot
biplot(X, type = "form")
```

<img src="man/figures/README-biplot-1.png" style="display: block; margin: auto;" />

``` r
## Highlight species
viz_individuals(
  x = X, 
  highlight = iris$Species, 
  color = c("#004488", "#DDAA33", "#BB5566")
)
## Add ellipses
viz_tolerance(
  x = X, 
  group = iris$Species, 
  level = 0.95,
  border = c("#004488", "#DDAA33", "#BB5566")
)

## Highlight petal length
viz_individuals(
  x = X,
  highlight = iris$Petal.Length,
  color = khroma::color("iridescent")(255), 
  size = c(1, 2),
)
```

<img src="man/figures/README-plot-ind-1.png" width="50%" /><img src="man/figures/README-plot-ind-2.png" width="50%" />

``` r
## Plot variables factor map
viz_variables(X)

## Scree plot
screeplot(X, eigenvalues = FALSE, cumulative = TRUE)
```

<img src="man/figures/README-plot-var-1.png" width="50%" /><img src="man/figures/README-plot-var-2.png" width="50%" />

## Contributing

Please note that the **dimensio** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
