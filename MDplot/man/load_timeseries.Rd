% (C) 2015 Christian Margreitter
% last update: 2017-02-21
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
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}. Note, that for \code{GROMACS} input, mulit-column data is supported.}
}
\value{
List of vectors, holding alternately indices and values.
}
\examples{
# GROMOS
load_timeseries( c( system.file( "extdata/timeseries_example_1.txt.gz",
                                 package = "MDplot" ),
                    system.file( "extdata/timeseries_example_2.txt.gz",
                                 package = "MDplot" ) ) )

# GROMACS
load_timeseries( c( system.file( "extdata/timeseries_example_GROMACS.txt.gz",
                                 package = "MDplot" ) ),
                 mdEngine = "GROMACS" )
}
\author{Christian Margreitter}
\keyword{timeseries}
