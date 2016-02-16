%  Copyright (C) 2015 Christian Margreitter
\name{load_TIcurve}
\alias{load_TIcurve}
\title{Loading function for thermodynamic integration function \code{\link{TIcurve}()}}
\description{
Returns a list of matrices holding three columns (lambda state point, value and error) for every file.}
\usage{
load_TIcurve( files,
              mdEngine = "GROMOS" )
}
\arguments{
  \item{files}{Vector of files (up to two) to be loaded.)}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# loading
load_TIcurve( c( system.file( "extdata/TIcurve_example.txt.gz", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{thermodynamic integration}
