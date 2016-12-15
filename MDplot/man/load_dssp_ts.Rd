%  Copyright (C) 2015 Christian Margreitter
\name{load_dssp_ts}
\alias{load_dssp_ts}
\title{Load DSSP timeseries}
\description{
Loads DSSP output files from a specified directory and combines it into a list suited for \code{\link{dssp_ts}()}.}
\usage{
load_dssp_ts( folder,
              filenames = NA,
              stride = 1,
              mdEngine = "GROMOS" )}
\arguments{
  \item{folder}{Folder, in which the DSSP output files are located.}
  \item{filenames}{Vector with filenames. Default file names are "3-Helix.out", "4-Helix.out", "5-Helix.out", "Bend.out", "Beta-Bridge.out", "Beta-Strand.out" and "Turn.out". Files not present are ignored.}
  \item{stride}{Allows to use only every xth snapshot.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine. Currently available: "GROMOS" and "GROMACS".}
}
\examples{
% see "extdata" for format information and file names
% GROMOS
load_dssp_ts( system.file( "extdata", package = "MDplot" ) )

% GROMACS
load_dssp_ts( folder = system.file( "extdata", package = "MDplot" ),
              filenames = "dssp_summary_example_GROMACS.txt.gz",
              mdEngine = "GROMACS" )
}
\author{Christian Margreitter}
\keyword{DSSP}
