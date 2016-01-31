%  Copyright (C) 2015 Christian Margreitter
\name{load_hbond}
\alias{load_hbond}
\title{Loading hydrogen bonds}
\description{
This function loads hydrogen bond information from a plain text file and stores them in a table.}
\usage{
load_hbond( path,
            mdEngine = "GROMOS" ) }
\arguments{
  \item{path}{Specified the path of the text input file.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# see "extdata/hbond_example.txt" for format information
load_hbond( system.file( "extdata/hbond_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{hbond}