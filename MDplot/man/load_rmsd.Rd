%  Copyright (C) 2015 Christian Margreitter
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
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
\dontrun{
load_rmsd( c( system.file( "extdata/rmsd1_example.txt.gz", package = "MDplot" ),
              system.file( "extdata/rmsd2_example.txt.gz", package = "MDplot" ) ) )
}}
\author{Christian Margreitter}
\keyword{rmsd}
