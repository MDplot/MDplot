%  Copyright (C) 2015 Christian Margreitter
\name{rmsf}
\alias{rmsf}
\title{Root-mean-square-fluctuation plot}
\description{Plot one (or more) rmsf files as produced by molecular dynamics packages.}
\usage{
rmsf( rmsfData,
      printLegend = TRUE,
      rmsfUnit = "nm",
      colours = NA,
      residuewise = FALSE,
      numberXLabels = 7,
      names = NA,
      range = NA,
      barePlot = FALSE,
      ... )}
\arguments{
  \item{rmsfData}{List of (alterating) atom numbers and rmsf values, as also produced by \code{MDplot::load_rmsf()} for example.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{rmsfUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{residuewise}{Boolean, specifying whether atoms or residues are plotted on the x-axis.}
  \item{numberXLabels}{Specifies, how many ticks are used on the x-axis.}
  \item{names}{Vector of the names of the trajectories.}
  \item{range}{Range of residues to be plotted.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\examples{\dontrun{
# two curves
rmsf( load_rmsf( c( system.file( "extdata/rmsf1_example.txt", package = "MDplot" ),
                    system.file( "extdata/rmsf2_example.txt", package = "MDplot" ) ) ) )
}}
\author{Christian Margreitter}
\keyword{Root-mean-square-fluctuation}
