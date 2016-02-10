%  Copyright (C) 2015 Christian Margreitter
\name{load_clusters}
\alias{load_clusters}
\title{Loading cluster information}
\description{
This function loads clusters from a plain text file and stores them in a matrix. The trajectories can be named by the user.}
\usage{
load_clusters( path,
               names = NA,
               mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Specified the path of the text input file.}
  \item{names}{Optional vector of trajectory names. Needs to be of the same length as the number of clusters to be plotted.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# see "extdata/clusters_example.txt" for format information
load_clusters( system.file( "extdata/clusters_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{Clusters}
