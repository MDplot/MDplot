% (C) 2015 Christian Margreitter
% last update: 2017-02-21
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
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}.}
}
\value{
Returns a list of (at least) \code{n}x3-matrices, each holding three columns:
\itemize{\item{[1]} lambda-points
\item{[2]} partial derivative of the Hamiltonian in respect to lambda at respective lambda
\item{[3]} error associated with partial derivative at respective lambda
}}
\examples{
# GROMOS
load_TIcurve( c( system.file( "extdata/TIcurve_example.txt.gz",
                              package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{thermodynamic integration}
