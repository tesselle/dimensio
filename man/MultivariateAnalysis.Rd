% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{MultivariateAnalysis}
\alias{MultivariateAnalysis}
\alias{.MultivariateAnalysis}
\alias{MultivariateAnalysis-class}
\title{Output of Multivariate Data Analysis}
\description{
A virtual S4 class to store the output of a multivariate data analysis.
}
\section{Slots}{

\describe{
\item{\code{data}}{A \code{\link{numeric}} \code{\link{matrix}}.}

\item{\code{dimension}}{An \code{\link{integer}} giving the dimension of the solution.}

\item{\code{singular_values}}{A \code{\link{numeric}} vector giving the singular values.}

\item{\code{rows}}{A \code{\linkS4class{MultivariateResults}} object.}

\item{\code{columns}}{A \code{\linkS4class{MultivariateResults}} object.}

\item{\code{extra}}{A \code{\link{list}} of extra variables.}
}}

\section{Subset}{

In the code snippets below, \code{x} is a \code{MultivariateAnalysis} object.
\describe{
\item{\code{x[[i]]}}{Extracts information from a slot selected by subscript \code{i}.
\code{i} is a length-one \code{\link{character}} vector.}
}
}

\seealso{
Other class: 
\code{\link{BootstrapCA-class}},
\code{\link{BootstrapPCA-class}},
\code{\link{CA-class}},
\code{\link{MCA-class}},
\code{\link{MultivariateBootstrap}},
\code{\link{MultivariateResults}},
\code{\link{MultivariateSummary}},
\code{\link{PCA-class}},
\code{\link{PCOA-class}}
}
\author{
N. Frerebeau
}
\concept{class}
\keyword{internal}
