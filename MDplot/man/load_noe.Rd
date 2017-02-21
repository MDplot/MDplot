% (C) 2016 Christian Margreitter
% last update: 2017-02-21
\name{load_noe}
\alias{load_noe}
\title{Loading NOE violations}
\description{
This function load one or more Nuclear-Overhauser-Effect (NOE) violation files. Its output can be feeded into function \code{\link{noe}()}. Note, that in case the number of used bins differ between files, the resulting matrix is automatically expanded to cover all bins.}
\usage{
load_noe( files,
          mdEngine = "GROMOS" )
}
\arguments{
  \item{files}{Vector of file paths to be loaded.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}.}
}
\value{
Returns a matrix, in which the first column holds the bin boundaries and the following columns represent the data from the respective input files, i.e. the number of violations in the respective bin.
}
\examples{
# GROMOS
load_noe( c( system.file( "extdata/noe_example_1.txt.gz",
                          package = "MDplot" ),
             system.file( "extdata/noe_example_2.txt.gz",
                          package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{hbond}