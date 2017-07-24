% (C) 2015 Christian Margreitter
% last update: 2017-07-04
\name{load_hbond_ts}
\alias{load_hbond_ts}
\title{Loading hydrogen bonds timeseries}
\description{
This function loads hydrogen bond timeseries information from a text file and stores it in a table. See function \code{\link{hbond_ts}()} for usage of the return value. In case, AMBER format is used as input, this functions' return value might also be used for function \code{\link{hbond}()}.}
\usage{
load_hbond_ts( path,
               mdEngine = "GROMOS" ) }
\arguments{
  \item{path}{Specifies the path of the input file.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
Returns a \code{n}x2-matrix, where the first column holds the time in snapshots and the second one the respective hydrogen bond identifier. Note, that function \code{\link{hbond_ts}()} requires additional input provided by function \code{\link{load_hbond}()} and that hydrogen bond identifiers have to match.
}
\examples{
# the examples are valid and wrapped in the "dontrun{}" environment for efficiency purposes only
# GROMOS
\dontrun{
load_hbond_ts( system.file( "extdata/hbond_ts_example.txt.gz", package = "MDplot" ) )}

# GROMACS
\dontrun{
load_hbond_ts( system.file( "extdata/hbond_ts_example_GROMACS.xpm.gz",
                            package = "MDplot" ),
               mdEngine = "GROMACS" )}

# AMBER
\dontrun{
load_hbond_ts( system.file( "extdata/hbond_ts_example_AMBER.txt.gz",
                            package = "MDplot" ),
               mdEngine = "AMBER" )}
}
\author{Christian Margreitter}
\keyword{hbond}