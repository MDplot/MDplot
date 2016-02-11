%  Copyright (C) 2015 Christian Margreitter
\name{ramachandran}
\alias{ramachandran}
\title{Ramachandran plot for two dihedral angles}
\description{
This plotting function divides a full rotation (360 degrees) into x- and y- bins and colors them according to the number of angle pairs that are provided in the input, a so-called Ramachandran plot.}
\usage{
ramachandran( dihedrals,
              xBins = 150,
              yBins = 150,
              heatFun = "norm", 
              structureAreas = c(),
              plotType = "sparse",
              printLegend = FALSE,
              heatUnits = NA,
              plotContour = FALSE,
              barePlot = FALSE,
              ... )}
\arguments{
  \item{dihedrals}{Matrix with angles (two columns) and optional column for heat values.}
  \item{xBins}{Number of bins (default: 150) used to plot (on the x-axis).}
  \item{yBins}{Number of bins (default: 150) used to plot (on the y-axis).}
  \item{heatFun}{Function selector for calculation of the colour, possible are either:
                 \itemize{
                   \item "norm" (default) for linear calculation, or
                   \item "log" for logarithmic calculation.
                         }
                }
  \item{structureAreas}{List of secondary structure areas, by default there are no regions specified.}
  \item{plotType}{Type of plot to be used, either "sparse" (default) or "fancy". The latter is a 3D representation.}
  \item{printLegend}{Boolean (default: false) specifying, whether a heat lengend is to be plotted or not.}
  \item{heatUnits}{String, containing a the unit of heat.}
  \item{plotContour}{Boolean (default: false) specifying, whether a contour should be plotted or no.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\examples{
\dontrun{
# load table and return it into plotting function "MDplot::ramachandran()"
# see "extdata/ramachandran_example.txt.gz" for format information
ramachandran( load_ramachandran( system.file( "extdata/ramachandran_example.txt.gz",
                                              package = "MDplot" ) ) )
}}
\author{Christian Margreitter}
\keyword{ramachandran}
\keyword{angles}
\keyword{dihedral}
