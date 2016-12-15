%  Copyright (C) 2015 Christian Margreitter
\name{load_dssp_summary}
\alias{load_dssp_summary}
\title{Load DSSP information}
\description{
Loads DSSP summary output files from a specified directory and combines it into a table.}
\usage{
load_dssp_summary( path,
                   mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Path to the input file.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine. Currently available: "GROMOS" and "GROMACS".}
}
\examples{
% see "extdata/dssp_summary_example.txt.gz" for format information and file names
% GROMOS
load_dssp_summary( system.file( "extdata/dssp_summary_example.txt.gz", package = "MDplot" ) )

% GROMACS
load_dssp_summary( system.file( "extdata/dssp_summary_example_GROMACS.txt.gz", package = "MDplot" ), mdEngine = "GROMACS" )
}
\author{Christian Margreitter}
\keyword{dssp}
