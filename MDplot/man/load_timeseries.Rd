% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{load_timeseries}
\alias{load_timeseries}
\title{Loading function for \code{\link{timeseries}()}}
\description{
Returns a list of vector pairs of datapoint indices and values.}
\usage{
load_timeseries( files )
}
\arguments{
  \item{files}{Vector of paths to input text files.}
}
\value{
List of vectors, holding alternately indices and values.
}
\examples{
load_timeseries( c( system.file( "extdata/timeseries_example_1.txt.gz",
                                 package = "MDplot" ),
                    system.file( "extdata/timeseries_example_2.txt.gz",
                                 package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{timeseries}
