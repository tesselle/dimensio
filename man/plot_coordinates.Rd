% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plot.R
\docType{methods}
\name{plot_coordinates}
\alias{plot_coordinates}
\alias{plot_rows}
\alias{plot_rows-method}
\alias{plot_columns}
\alias{plot_columns-method}
\alias{plot_rows,MultivariateAnalysis-method}
\alias{plot,CA,missing-method}
\alias{plot_columns,CA-method}
\alias{plot,PCA,missing-method}
\alias{plot_columns,PCA-method}
\alias{plot_columns,BootstrapPCA-method}
\title{Visualize Factor Map}
\usage{
plot_rows(object, ...)

plot_columns(object, ...)

\S4method{plot_rows}{MultivariateAnalysis}(
  object,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)

\S4method{plot}{CA,missing}(
  x,
  margin = c(1, 2),
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)

\S4method{plot_columns}{CA}(
  object,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)

\S4method{plot}{PCA,missing}(
  x,
  margin = 1,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)

\S4method{plot_columns}{PCA}(
  object,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)

\S4method{plot_columns}{BootstrapPCA}(
  object,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  highlight = NULL,
  group = NULL
)
}
\arguments{
\item{object, x}{A \code{\linkS4class{CA}} or \code{\linkS4class{PCA}} object.}

\item{...}{Currently not used.}

\item{axes}{A length-two \code{\link{numeric}} vector giving the dimensions to be
plotted.}

\item{active}{A \code{\link{logical}} scalar: should the active observations be
plotted?}

\item{sup}{A \code{\link{logical}} scalar: should the supplementary observations be
plotted?}

\item{highlight}{A \code{\link{character}} string giving XXX.
It must be one of "\code{coordinates}", "\code{contributions}" or "\code{cos2}".
Any unambiguous substring can be given.
If \code{NULL} (the default), no highlighting is applied.}

\item{group}{A vector of categories specifying the categorical variable from
which to color the individuals (only used if \code{highlight} is \code{NULL}; see
below).}

\item{margin}{A length-one \code{\link{numeric}} vector giving the subscript which the
data will be returned: \code{1} indicates individuals/rows (the default), \code{2}
indicates variables/columns, \code{c(1, 2)} indicates rows and columns (\code{CA}).}
}
\description{
Plots factor map.
}
\section{Aesthetic}{

Point shapes and line types are set whether an observation is a
row/individual or a column/variable and is active or supplementary.

Colors are set according to \code{highlight} and \code{group}:
\itemize{
\item{If \code{highlight} is not \code{NULL}, the color gradient will vary according
to the value of the selected parameter.}
\item{If \code{group} is a \code{numeric} vector, the color gradient and size will
vary by the value of \code{group}.}
\item{If \code{group} is not a \code{numeric} vector, the colors will be mapped to
the levels of \code{group}.}
\item{If both are \code{NULL} (the default), then the same rule as for shapes is
used.}
}
}

\examples{
## Load data
data("zuni", package = "folio")

## Compute correspondence analysis
X <- ca(zuni)

## Plot observations
plot(X)

## Screeplot
plot_variance(X)
}
\seealso{
\code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}

Other plot: 
\code{\link{plot_contributions}()},
\code{\link{plot_eigenvalues}}
}
\author{
N. Frerebeau
}
\concept{plot}
