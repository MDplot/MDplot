%  Copyright (C) 2015 Christian Margreitter
\name{clusters}
\alias{clusters}
\title{Cluster bar plot}
\description{
This function plots clusters over a set of trajectories as joint, coloured bar plots. The clusters are sorted beginning with the most populated one and then in descending order. Selections can be made.}
\usage{
clusters( clusters,
          clustersNumber = NA,
          legendTitle = "trajectories",
          ... )}
\arguments{
  \item{dihedrals}{Matrix with clusters. Trajectories in rows, clusters in columns.}
  \item{clustersNumber}{When specified, only this number of clusters is shown.}
  \item{legendTitle}{The title of the legend.}
}
\examples{
# see "extdata/clusters_example.txt" for format information
MDplot::clusters( MDplot::load_clusters( system.file( "extdata/clusters_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{Clusters}