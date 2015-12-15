%  Copyright (C) 2015 Christian Margreitter
\name{cluster_ts}
\alias{cluster_ts}
\title{Cluster timeseries plot}
\description{
This function plots distributions between clusters over trajectories. On top, the overall distribution is given - the timeseries is given at the bottom. The clusters are sorted beginning with the most populated one and then descending. Selections can be made, clusters that are not selected do also not appear in the timeseries plot (white areas).}
\usage{
clusters_ts <- function( clustersDataTS,
                         clustersNumber = NA,
                         selectTraj = NA,
                         selectTime = NA,
                         printNanoseconds = FALSE,
                         snapshotsPerNS = 1000,
                         ... )}
\arguments{
  \item{clustersDataTS}{List of cluster information as provided by \code{MDplot::load_clusters_ts()}, the associated loading function.}
  \item{clustersNumber}{Integer, specifying the number of clusters that is to be plotted.}
  \item{selectTraj}{Vector of indices of trajectories that are to be plotted (as given in the input file).}
  \item{selectTime}{Range of time in snapshots, which is to be plotted.}
  \item{printNanoseconds}{Boolean, that specifies whether the time shall be printed in nanoseconds or not.}
  \item{snapshotsPerNS}{If \code{printNanoseconds} is \code{TRUE}, this integer specifies the number of snapshots per nanosecond.}
  \item{...}{Graphical parameters.}
}
\examples{
# see "extdata/cluster_ts_example.txt" for format information
MDplot::cluster_ts( MDplot::load_cluster_ts( system.file( "extdata/cluster_ts_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{cluster_ts}