% (C) 2015 Christian Margreitter
% last update: 2021-05-18
\name{clusters_ts}
\alias{clusters_ts}
\title{Cluster timeseries plot}
\description{
This function plots distributions between clusters over time. In the top sub-plot, the overall distribution is given, while the timeseries is given at the bottom. The clusters are sorted beginning with the most populated one and then in descending order. Selections can be made and clusters that are not selected do also not appear in the timeseries plot (white areas).}
\usage{
clusters_ts( clustersDataTS,
             clustersNumber = NA,
             selectTraj = NA,
             selectTime = NA,
             timeUnit = NA,
             snapshotsPerTimeInt = 1000,
             colourPalette = NA,
             ... )}
\arguments{
  \item{clustersDataTS}{List of cluster information as provided by \code{\link{load_clusters_ts}()}, the associated loading function.}
  \item{clustersNumber}{Integer, specifying the number of clusters that is to be plotted.}
  \item{selectTraj}{Vector of indices of trajectories that are to be plotted (as given in the input file).}
  \item{selectTime}{Range of time in snapshots, which is to be plotted.}
  \item{timeUnit}{Abbreviation of time unit.}
  \item{snapshotsPerTimeInt}{Specifies, how many snapshots make up one time unit.}
  \item{colourPalette}{A \code{colorRampPalette} that is used for colouring the clusters.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a summary \code{(n+1)}x\code{m}-matrix with \code{n} being the number of input trajectories and \code{m} the number of different clusters (which have been plotted). Each element in the matrix holds the number of snapshots, the respective cluster occured in the respective trajectory. In addition, the first line is the overall summary counted over all trajectories.
}
\examples{
# GROMOS (see load_clusters_ts() for other input possibilities)
clusters_ts( load_clusters_ts( system.file( "extdata/clusters_ts_example.txt.gz",
                               package = "MDplot" ),
                               lengths = c( 4000, 4000, 4000, 4000, 4000, 4000 ) ),
             clustersNumber = 7 )
}
\author{Christian Margreitter}
\keyword{clusters_ts}
