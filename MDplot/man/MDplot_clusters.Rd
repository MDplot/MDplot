%  Copyright (C) 2015 Christian Margreitter
\name{MDplot_clusters}
\alias{MDplot_clusters}
\title{Cluster bar plot}
\description{
This function plots clusters over a set of trajectories as joint, coloured bar plots. The clusters are sorted beginning with the most populated one and then descending. Selections can be made.}
\usage{
MDplot_clusters( clusters,
                 clustersNumber = NA,
                 legendTitle = "trajectories",
                 ownTrajectoryNames = FALSE,
                 ... )}
\arguments{
  \item{dihedrals}{Matrix with clusters. Trajectories in rows, clusters in columns.}
  \item{clustersNumber}{When specified, only this number of clusters is shown.}
  \item{legendTitle}{The title of the legend.}
  \item{ownTrajectoryNames}{Boolean, specifying whether rownames of \code{dihedrals} are used in the legend.}
}
\examples{
# see "extdata/MDplot_clusters_example.txt" for format information
MDplot_clusters( MDplot_load_clusters( system.file( "extdata/MDplot_clusters_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{Clusters}