%  Copyright (C) 2015 Christian Margreitter
\name{MDplot_ramachandran}
\alias{MDplot_ramachandran}
\title{Ramachandran plot for two dihedral angles}
\description{
This plotting function divides a full rotation (360 degrees) into x- and y- bins and colors them according to the number of angle pairs that are provided in the input, a so-called Ramachandran plot.}
\usage{
MDplot_ramachandran( MAT_dihedrals,
                     xBins = 150,
                     yBins = 150,
                     heatFun = "norm", 
                     structureAreas = c(),
                     plotType = "comic",
                     printLegend = FALSE,
                     heatUnits = NA,
                     plotContour = FALSE,
                     ... )}
\arguments{
  \item{MAT_dihedrals}{Matrix with angles (two columns) and optional column for heat values.}
  \item{xBins}{Number of bins (default: 150) used to plot (on the x-axis).}
  \item{yBins}{Number of bins (default: 150) used to plot (on the y-axis).}
  \item{heatFun}{Function selector for calculation of the color, possible are either:
                 \itemize{
                   \item "norm" (default) for linear calculation, or
                   \item "log"" for logarithmic calculation.
                         }
                }
  \item{structureAreas}{List of secondary structure areas, by default there are no regions specified.}
  \item{plotType}{Type of plot to be used, either "comic" (default), "sparse" or "fancy".}
  \item{printLegend}{Boolean (default: false) specifying, whether a heat lengend is to be plotted or not.}
  \item{heatUnits}{String, containing a the unit of heat.}
  \item{plotContour}{Boolean (default: false) specifying, whether a contour should be plotted or no.}
}
\examples{
# load table and return it into plotting function "MDplot_ramachandran()"
# see "extdata/MDplot_ramachandran_example.txt" for format information
MDplot_ramachandran( MDplot_load_ramachandran( system.file( "extdata/MDplot_ramachandran_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{Ramachandran}