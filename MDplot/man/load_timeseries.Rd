%  Copyright (C) 2015 Christian Margreitter
\name{load_timeseries}
\alias{load_timeseries}
\title{Loading function for \code{\link{timeseries}()}}
\description{
Returns a list of vector pairs of datapoint indices and values.}
\usage{
load_timeseries( files,
                 mdEngine = "GROMOS" )
}
\arguments{
  \item{files}{Vector of paths to input text files.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
\dontrun{
load_timeseries( c( system.file( "extdata/timeseries_example.txt.gz", package = "MDplot" ) )
}}
\author{Christian Margreitter}
\keyword{timeseries}
