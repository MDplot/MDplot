% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{timeseries}
\alias{timeseries}
\title{General timeseries plotting function}
\description{Plot one (or more) timeseries plots.}
\usage{
timeseries( tsData,
            printLegend = TRUE,
            snapshotsPerTimeInt = 1000,
            timeUnit = "ns",
            valueName = NA,
            valueUnit = NA,
            colours = NA,
            names = NA,
            legendPosition = "bottomright",
            barePlot = FALSE,
            ... )}
\arguments{
  \item{tsData}{List of (alternating) indices and response values, as also produced by \code{\link{load_timeseries}()} for example.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{snapshotsPerTimeInt}{Number, specifying how many snapshots are within one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{valueName}{Name of response variable.}
  \item{valueUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{names}{Vector of the names of the trajectories.}
  \item{legendPosition}{Indicate position of legend: either "\code{bottomright}", "\code{bottomleft}", "\code{topleft}" or "\code{topright}".}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a list of list, the latter each holding for every data input series:
\itemize{\item{\code{minValue}} The minimum value over the whole set.
\item{\code{maxValue}} The maximum value over the whole set.
\item{\code{meanValue}} The mean value over the whole set.
\item{\code{sd}} The standard deviation over the whole set.
}}
\examples{
timeseries( load_timeseries( c( system.file( "extdata/timeseries_example_1.txt.gz",
                                             package = "MDplot" ),
                                system.file( "extdata/timeseries_example_2.txt.gz",
                                             package = "MDplot" ) ) ),
            snapshotsPerTimeInt = 100 )
}
\author{Christian Margreitter}
\keyword{timeseries}
