%  Copyright (C) 2015 Christian Margreitter
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
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine. Currently available: "GROMOS" and "GROMACS".}
}
\examples{
% GROMOS
load_rmsf( c( system.file( "extdata/rmsf1_example.txt.gz", package = "MDplot" ),
              system.file( "extdata/rmsf2_example.txt.gz", package = "MDplot" ) ) )

% GROMACS
load_rmsf( c( system.file( "extdata/rmsf_example_GROMACS.txt.gz",
                           package = "MDplot" ) ),
          mdEngine = "GROMACS" )
}
\author{Christian Margreitter}
\keyword{rmsf}
