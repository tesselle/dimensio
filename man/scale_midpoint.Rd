% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotations.R
\name{scale_midpoint}
\alias{scale_midpoint}
\title{Rescale Continuous Vector (minimum, midpoint, maximum)}
\usage{
scale_midpoint(x, to = c(0, 1), from = range(x, finite = TRUE), midpoint = 0)
}
\arguments{
\item{x}{A \code{\link{numeric}} vector.}

\item{to}{A length-two \code{\link{numeric}} vector specifying the output range.}

\item{from}{A length-two \code{\link{numeric}} vector specifying the input range.}

\item{midpoint}{A length-one \code{\link{numeric}} vector specifying the mid-point of
input range.}
}
\value{
A \code{\link{numeric}} vector.
}
\description{
Rescales continuous vector to have specified minimum, midpoint and maximum.
}
\seealso{
Other scales: 
\code{\link{scale_range}()}
}
\concept{scales}
\keyword{internal}
