% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimensio-internal.R
\name{prepare}
\alias{prepare}
\title{Prepare Data for Plotting}
\usage{
prepare(
  x,
  margin,
  ...,
  axes = c(1, 2),
  active = TRUE,
  sup = TRUE,
  principal = TRUE,
  highlight = NULL,
  col = NULL,
  bg = NULL,
  pch = 16,
  cex = NULL,
  lty = NULL,
  lwd = NULL,
  alpha = FALSE
)
}
\arguments{
\item{x}{A \code{\linkS4class{MultivariateAnalysis}} object.}

\item{margin}{A length-one \code{\link{numeric}} vector giving the subscript
which the data will be returned: \code{1} indicates individuals/rows (the
default), \code{2} indicates variables/columns.}

\item{...}{Currently not used.}

\item{axes}{A length-two \code{\link{numeric}} vector giving the dimensions to be
plotted.}

\item{active}{A \code{\link{logical}} scalar: should the active observations be
plotted?}

\item{sup}{A \code{\link{logical}} scalar: should the supplementary observations be
plotted?}

\item{principal}{A \code{\link{logical}} scalar: should principal coordinates be
returned? If \code{FALSE}, standard coordinates are returned.}

\item{highlight}{A vector specifying the information to be highlighted.
If \code{NULL} (the default), no highlighting is applied. If a single \code{character}
string is passed, it must be the name of a categorical variable, or one of
"\code{observation}", "\code{mass}", "\code{sum}", "\code{contribution}" or "\code{cos2}"
(see \code{\link[=augment]{augment()}}).}

\item{col}{The colors for lines and points.}

\item{bg}{The background color for the open plot symbols given by \code{pch = 21:25}.}

\item{pch}{A vector of plotting characters or symbols. This can either be
a single character or an integer code for one of a set of graphics symbols.}

\item{cex}{A numerical vector giving the amount by which plotting characters
and symbols should be scaled relative to the default.}

\item{lty, lwd}{A specification for the line type and width.}
}
\value{
A \code{\link{data.frame}} with the following columns:
\describe{
\item{\code{x}}{Coordinates along x.}
\item{\code{y}}{Coordinates along y.}
\item{\code{z}}{Variable to be highlighted.}
\item{\code{label}}{Label.}
\item{\code{col}}{Color for lines and points.}
\item{\code{bg}}{Background color.}
\item{\code{pch}}{Symbol.}
\item{\code{cex}}{Symbol size.}
\item{\code{lty}}{Line type.}
\item{\code{lwd}}{Line width.}
}
}
\description{
Prepare Data for Plotting
}
\author{
N. Frerebeau
}
\keyword{internal}
