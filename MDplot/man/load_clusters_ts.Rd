%  Copyright (C) 2015 Christian Margreitter
\name{load_clusters_ts}
\alias{load_clusters_ts}
\title{Loading time-series cluster information}
\description{
This function loads the time-series information for clusters from a plain text file and stores them in a list. The trajectories can be named by the user.}
\usage{
load_clusters_ts( path,
                  lenghts,  
                  names = NA,
                  mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Specified the path of the text input file.}
  \item{lenghts}{Mandatory vector holding the number of snapshots for the respective trajectories.}
  \item{names}{Optional vector of trajectory names. Needs to be of the same length as the number of clusters to be plotted.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# see "extdata/clusters_ts_example.txt" for format information
MDplot::load_clusters_ts( system.file( "extdata/clusters_ts_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{Clusters}