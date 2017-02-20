% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_dssp_ts}
\alias{load_dssp_ts}
\title{Load DSSP timeseries}
\description{
Loads DSSP output files from a specified directory (GROMOS) or a specified file (GROMACS, AMBER) and combines it into a list suited as input for \code{\link{dssp_ts}()}.}
\usage{
load_dssp_ts( folder,
              filenames = NA,
              stride = 1,
              mdEngine = "GROMOS" )}
\arguments{
  \item{folder}{Folder, in which the DSSP output files are located.}
  \item{filenames}{Vector with filenames. GROMOS: default file names are "3-Helix.out", "4-Helix.out", "5-Helix.out", "Bend.out", "Beta-Bridge.out", "Beta-Strand.out" and "Turn.out". Files not present are ignored. GROMACS and AMBER: just give the appropriate filename (see examples below).}
  \item{stride}{Allows to use only every \code{x}th snapshot.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
Returns a list, where every element represents a secondary structure motif holding the following information:
\item{name}{The name of the secondary structure motif.}
\item{values}{Holds one vector each for the \code{x} and \code{y} axis:
              \itemize{\item{\code{x}} Snapshots, at which the respective motif has been identified.
              \item{\code{y}} Residues, for which the respective motif has been identified.}}
}
\examples{
# GROMOS
load_dssp_ts( system.file( "extdata/dssp_ts_example", package = "MDplot" ) )

# GROMACS
load_dssp_ts( folder = system.file( "extdata", package = "MDplot" ),
              filenames = "dssp_example_GROMACS.txt.gz",
              mdEngine = "GROMACS" )

# AMBER
load_dssp_ts( folder = system.file( "extdata", package = "MDplot" ),
              filenames = "dssp_ts_example_AMBER.txt.gz",
              mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{DSSP}
