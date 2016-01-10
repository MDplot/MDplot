%  Copyright (C) 2015 Christian Margreitter
\name{load_dssp_summary}
\alias{load_dssp_summary}
\title{Load DSSP information}
\description{
Loads dssp summary output files from a specified directory and combines it into a list.}
\usage{
load_dssp_summary( path )
}
\arguments{
  \item{path}{Folder, in which the dssp output files are located.}
}
\examples{
# see "extdata" for format information and file names
MDplot::load_dssp_summary( system.file( "extdata", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{dssp}