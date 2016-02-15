%  Copyright (C) 2015 Christian Margreitter
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
  \item{tsData}{List of (alterating) indices and response values, as also produced by \code{load_timeseries()} for example.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{snapshotsPerTimeInt}{Number, specifying how many snapshots are within one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{valueName}{Name of response variable.}
  \item{valueUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{names}{Vector of the names of the trajectories.}
  \item{legendPosition}{Indicate position of legend: either "bottomright", "bottomleft", "topleft" or "topright".}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\examples{\dontrun{
# two curves
timeseries( load_timeseries( system.file( "extdata/timeseries_example.txt.gz",
                                          package = "MDplot" ) ),
            snapshotsPerTimeInt = 100 )
}}
\author{Christian Margreitter}
\keyword{timeseries}
