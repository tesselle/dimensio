# dimensio 0.2.1

## Bugfixes & changes
* Fix row names in `build_results()`. When initializing a `MultivariateResults` object with supplementary observations, the row names of the `standard` and `contributions` matrices were incorrect (computation moves all supplementary points at the end of the results).

# dimensio 0.2.0

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4709122.svg)](https://doi.org/10.5281/zenodo.4709122)

## New classes and methods
* Add `bootstrap()` for partial bootstrap analysis.
* Add `BootstrapCA` and `BootstrapPCA`: S4 classes to store partial bootstrap analysis.

## Bugfixes & changes
* Rename `plot_individuals()` and `plot_variables()` to `plot_rows()` and `plot_columns()`.
* Remove `plot_eigenvalues()`.

# dimensio 0.1.0

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4478531.svg)](https://doi.org/10.5281/zenodo.4478531)

* Prelease.
