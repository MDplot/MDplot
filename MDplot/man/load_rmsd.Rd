% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_rmsd}
\alias{load_rmsd}
\title{Loading function for \code{\link{rmsd}()}}
\description{
Returns a list of vector pairs of datapoint indices and RMSD values.}
\usage{
load_rmsd( files,
           mdEngine = "GROMOS" )
}
\arguments{
  \item{files}{Vector of paths to input text files.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
A list of vectors, alternately holding indices and their respective values.
}
\examples{
# GROMOS
load_rmsd( c( system.file( "extdata/rmsd1_example.txt.gz", package = "MDplot" ),
              system.file( "extdata/rmsd2_example.txt.gz", package = "MDplot" ) ) )

# GROMACS
load_rmsd( c( system.file( "extdata/rmsd_example_GROMACS.txt.gz",
                           package = "MDplot" ) ),
           mdEngine = "GROMACS" )

# AMBER
load_rmsd( c( system.file( "extdata/rmsd_example_AMBER.txt.gz",
                           package = "MDplot" ) ),
           mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{rmsd}
