% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biplot.R
\name{viz_biplot}
\alias{viz_biplot}
\title{Build a Biplot}
\usage{
viz_biplot(
  coord_row,
  coord_col,
  ...,
  rows = TRUE,
  columns = TRUE,
  labels = c("rows", "columns", "individuals", "variables"),
  xlim = NULL,
  ylim = NULL,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  axes = TRUE,
  frame.plot = axes,
  ann = graphics::par("ann"),
  panel.first = NULL,
  panel.last = NULL,
  legend = list(x = "topleft")
)
}
\arguments{
\item{coord_row}{A \code{\link{data.frame}} returned by \code{\link[=prepare_plot]{prepare_plot()}}.}

\item{coord_col}{A \code{\link{data.frame}} returned by \code{\link[=prepare_plot]{prepare_plot()}}.}

\item{rows}{A \code{\link{logical}} scalar: should the rows be drawn?}

\item{columns}{A \code{\link{logical}} scalar: should the columns be drawn?}

\item{labels}{A \code{\link{character}} vector specifying whether
"\code{rows}"/"\code{individuals}" and/or "\code{columns}"/"\code{variables}" names must be
drawn. Any unambiguous substring can be given.}

\item{xlim}{A length-two \code{\link{numeric}} vector giving the x limits of the plot.
The default value, \code{NULL}, indicates that the range of the
\link[=is.finite]{finite} values to be plotted should be used.}

\item{ylim}{A length-two \code{\link{numeric}} vector giving the y limits of the plot.
The default value, \code{NULL}, indicates that the range of the
\link[=is.finite]{finite} values to be plotted should be used.}

\item{main}{A \code{\link{character}} string giving a main title for the plot.}

\item{sub}{A \code{\link{character}} string giving a subtitle for the plot.}

\item{xlab, ylab}{A \code{\link{character}} vector giving the x and y axis labels.}

\item{axes}{A \code{\link{logical}} scalar: should axes be drawn on the plot?}

\item{frame.plot}{A \code{\link{logical}} scalar: should a box be drawn around the
plot?}

\item{ann}{A \code{\link{logical}} scalar: should the default annotation (title and x
and y axis labels) appear on the plot?}

\item{panel.first}{An \code{expression} to be evaluated after the plot axes are
set up but before any plotting takes place. This can be useful for drawing
background grids.}

\item{panel.last}{An \code{expression} to be evaluated after plotting has taken
place but before the axes, title and box are added.}

\item{legend}{A \code{\link{list}} of additional arguments to be passed to
\code{\link[graphics:legend]{graphics::legend()}}; names of the list are used as argument names.
If \code{NULL}, no legend is displayed.}
}
\description{
Build a Biplot
}
\author{
N. Frerebeau
}
\keyword{internal}
