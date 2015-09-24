%  Copyright (C) 2015 Christian Margreitter
\name{MDplot_load_ramachandran}
\alias{MDplot_load_ramachandran}
\title{Ramachandran plot for two dihedral angles}
\description{
This plotting function divides a full rotation (360 degrees) into x- and y- bins and colors them according to the number of angle pairs that are provided in the input, a so-called Ramachandran plot.}
\usage{
MDplot_ramachandran( MAT_dihedrals,
                     xbins = 450,
                     ybins = 450,
                     heatFun = "log", 
                     LIST_areas = c(),
                     ... )}
\arguments{
  \item{MAT_dihedrals}{Matrix with angles (two rows maximum).}
  \item{xbins}{Number of bins used to plot (on the x-axis).}
  \item{ybins}{Number of bins used to plot (on the y-axis).}
  \item{heatFun}{Function selector for calculation of the color, possible are either:
                 \itemize{
                   \item "log"" for logarithmic calculation, or
                   \item "norm" for linear calculation
                         }
                }
}
\examples{
# load table and return it into plotting function "MDplot_ramachandran()"
# see "extdata/MDplot_ramachandran_example.txt" for format information
MDplot_ramachandran( MDplot_load_ramachandran( system.file( "extdata/MDplot_ramachandran_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{Ramachandran}