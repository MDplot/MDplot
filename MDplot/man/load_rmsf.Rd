% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_rmsf}
\alias{load_rmsf}
\title{Loading function for \code{\link{rmsf}()}}
\description{
Returns a list of vector pairs of datapoint indices and RMSF values.}
\usage{
load_rmsf( files,
           mdEngine = "GROMOS" )
}
\arguments{
  \item{files}{Vector of paths to input text files.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
A list of vectors, alternately holding atom indices and their respective values.
}
\examples{
# GROMOS
load_rmsf( c( system.file( "extdata/rmsf1_example.txt.gz", package = "MDplot" ),
              system.file( "extdata/rmsf2_example.txt.gz", package = "MDplot" ) ) )

# GROMACS
load_rmsf( c( system.file( "extdata/rmsf_example_GROMACS.txt.gz",
                           package = "MDplot" ) ),
          mdEngine = "GROMACS" )

# AMBER
load_rmsf( c( system.file( "extdata/rmsf_example_AMBER.txt.gz",
                           package = "MDplot" ) ),
          mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{rmsf}
