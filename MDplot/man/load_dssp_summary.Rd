%  Copyright (C) 2015 Christian Margreitter
\name{load_dssp_summary}
\alias{load_dssp_summary}
\title{Load DSSP information}
\description{
Loads dssp summary output files from a specified directory and combines it into a list.}
\usage{
load_dssp_summary( path,
                   mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Path to the input file.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# see "extdata" for format information and file names
MDplot::load_dssp_summary( system.file( "extdata/dssp_summary_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{dssp}