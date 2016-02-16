%  Copyright (C) 2015 Christian Margreitter
\name{clusters_ts}
\alias{clusters_ts}
\title{Cluster timeseries plot}
\description{
This function plots distributions between clusters over time. In the top sub-plot, the overall distribution is given, while the timeseries is given at the bottom. The clusters are sorted beginning with the most populated one and then in descending order. Selections can be made, clusters that are not selected do also not appear in the timeseries plot (white areas).}
\usage{
clusters_ts( clustersDataTS,
             clustersNumber = NA,
             selectTraj = NA,
             selectTime = NA,
             timeUnit = NA,
             snapshotsPerTimeInt = 1000,
             ... )}
\arguments{
  \item{clustersDataTS}{List of cluster information as provided by \code{\link{load_clusters_ts}()}, the associated loading function.}
  \item{clustersNumber}{Integer, specifying the number of clusters that is to be plotted.}
  \item{selectTraj}{Vector of indices of trajectories that are to be plotted (as given in the input file).}
  \item{selectTime}{Range of time in snapshots, which is to be plotted.}
  \item{timeUnit}{Abbreviation of time unit.}
  \item{snapshotsPerTimeInt}{Specifies, how many snapshots make up one time unit.}
  \item{...}{Additional arguments (ellipsis).}
}
\examples{
# see "extdata/clusters_ts_example.txt.gz" for format information
clusters_ts( load_clusters_ts( system.file( "extdata/clusters_ts_example.txt.gz",
                               package = "MDplot" ),
                               lengths = c( 4000, 4000, 4000, 4000, 4000, 4000 ) ),
             clustersNumber = 7 )
}
\author{Christian Margreitter}
\keyword{clusters_ts}
