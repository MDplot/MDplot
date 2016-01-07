%  Copyright (C) 2015 Christian Margreitter
\name{MDplot_load_XRMSD}
\alias{MDplot_load_XRMSD}
\title{Loading function for \link{MDplot_XRMSD}}
\description{
Loads matrix information from the specified file. Caution: the function is in beta-state and extremely sensitive to format changes.}
\usage{
MDplot_load_XRMSD <- function( STRING_path,
                               INT_skipBeginning = 8,
                               INT_factor = 10000 )
\arguments{
  \item{STRING_path}{Specifies the input file.}
  \item{INT_skipBeginning}{Determines, how many lines are ignored (format dependent).}
  \item{INT_factor}{In case the RMSD values are given in \deqn{nm * INT_factor}, the factor can be specified. If the unit is already in nanometers, 1 is the appropriate value.}
}
\examples{
# load and return matrix
MDplot_load_XRMSD( system.file( "extdata/MDplot_DSSP_summary_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{DSSP}