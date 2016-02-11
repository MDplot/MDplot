%  Copyright (C) 2015 Christian Margreitter
\name{rmsd}
\alias{rmsd}
\title{Root-mean-square-deviation plot}
\description{Plot one (or more) RMSD files as produced by molecular dynamics packages.}
\usage{
rmsd( rmsdData,
      printLegend = TRUE,
      factor = 1000,
      timeUnit = "ns",
      rmsdUnit = "nm",
      colours = NA,
      names = NA,
      barePlot = FALSE,
      ... )}
\arguments{
  \item{rmsdData}{List of (alterating) indices and rmsd values, as also produced by \code{MDplot::load_rmsd()} for example.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{factor}{Number, specifying how many snapshots are within one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{rmsdUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{names}{Vector of the names of the trajectories.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\examples{\dontrun{
# two curves
rmsd( load_rmsd( c( system.file( "extdata/rmsd1_example.txt.gz", package = "MDplot" ),
                    system.file( "extdata/rmsd2_example.txt.gz", package = "MDplot" ) ) ) )
}}
\author{Christian Margreitter}
\keyword{Root-mean-square-deviation}
