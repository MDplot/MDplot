% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_clusters_ts}
\alias{load_clusters_ts}
\title{Loading timeseries cluster information}
\description{
This function loads the timeseries information for clusters from a plain text file and stores them in a list. The trajectories can be named by the user.}
\usage{
load_clusters_ts( path,
                  lengths,  
                  names = NA,
                  mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Specifies the path of the text input file.}
  \item{lengths}{Mandatory vector holding the number of snapshots for the respective trajectories (e.g. when three trajectories of 3000 snapshots each have been analysed together: \code{lengths = c(3000,3000,300)}).}
  \item{names}{Optional vector of trajectory names. If provided, needs to be of the same length as the number of clusters to be plotted.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
Returns a list of name-cluster lists, which consist of:
\item{[1]}{Trajectory name.}
\item{[2]}{Vector of cluster numbers, where \code{0} indicates a structure not belonging to a cluster specified.}
}
\examples{
# GROMOS
load_clusters_ts( system.file( "extdata/clusters_ts_example.txt.gz", package = "MDplot" ),
                  lengths = c( 4000, 4000, 4000, 4000, 4000, 4000 ) )

# GROMACS
load_clusters_ts( system.file( "extdata/clusters_example_GROMACS.txt.gz", package = "MDplot" ),
                  mdEngine = "GROMACS", lengths = c( 1001, 1001 ) )

# AMBER
load_clusters_ts( system.file( "extdata/clusters_ts_example_AMBER.txt.gz", package = "MDplot" ),
                  mdEngine = "AMBER", lengths = c( 50, 50 ) )
}
\author{Christian Margreitter}
\keyword{Clusters}