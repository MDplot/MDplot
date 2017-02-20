% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{clusters}
\alias{clusters}
\title{Cluster bar plot}
\description{
This function plots clusters over a set of trajectories as joint, coloured bar plots. The clusters are sorted beginning with the most populated one in descending order.}
\usage{
clusters( clusters,
          clustersNumber = NA,
          legendTitle = "trajectories",
          barePlot = FALSE,
          ... )}
\arguments{
  \item{clusters}{Matrix with clusters: trajectories are given in row-wise, clusters in column-wise fashion as provided by \code{\link{load_clusters}()}, the associated loading function.}
  \item{clustersNumber}{When specified, only these first clusters are shown.}
  \item{legendTitle}{The title of the legend.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a \code{n}x\code{m}-matrix with \code{n} being the number of input trajectories and \code{m} the number of different clusters. Each element in the matrix holds the number of snapshots, the respective cluster occured in the respective trajectory.
}
\examples{
# GROMOS (see load_clusters() for other input possibilities)
clusters( load_clusters( system.file( "extdata/clusters_example.txt.gz",
                                      package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{Clusters}
