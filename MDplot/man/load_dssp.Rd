% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_dssp}
\alias{load_dssp}
\title{Load DSSP information}
\description{
Loads DSSP summary output files from a specified file and combines it into a table. This table may be used as input for function \code{\link{dssp}()}}
\usage{
load_dssp( path,
           mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Path to the input file.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
Returns matrix, where the first column is the residue-number and the remaining ones denote secondary structure classes. Residues are given row-wise and values range from \code{0} to \code{100} percent.
}
\examples{
# GROMOS
load_dssp( system.file( "extdata/dssp_example.txt.gz", package = "MDplot" ) )

# GROMACS
load_dssp( system.file( "extdata/dssp_example_GROMACS.txt.gz",
                        package = "MDplot" ), mdEngine = "GROMACS" )

# AMBER (same input as for load_dssp_ts())
load_dssp( system.file( "extdata/dssp_ts_example_AMBER.txt.gz",
                        package = "MDplot" ), mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{dssp}
