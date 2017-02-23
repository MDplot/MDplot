% (C) 2015 Christian Margreitter
% last update: 2017-02-21
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
              plotContour = FALSE,
              barePlot = FALSE,
              ... )}
\arguments{
  \item{dihedrals}{Matrix with angles (two columns).}
  \item{xBins}{Number of x-axis bins to be plotted.}
  \item{yBins}{Number of y-axis bins to be plotted.}
  \item{heatFun}{Function selector for calculation of the colour, possible are either:
                 \itemize{
                   \item "norm" (default) for linear calculation, or
                   \item "log" for logarithmic calculation.
                         }
                }
  \item{structureAreas}{List of areas, which are plotted as black lines.}
  \item{plotType}{Type of plot to be used, either "sparse" (default, using function \code{\link{hist2d}()}), "comic" (own binning, use for very few datapoints) or "fancy" (3D, using function \code{\link{persp}()}).}
  \item{printLegend}{Boolean, specifying whether a heat legend is to be plotted or not.}
  \item{plotContour}{Boolean, specifying whether a contour should be plotted or no.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a list of binned dihedral angle occurrences.
}
\examples{
# GROMOS (see load_rmsd() for other input possibilities)
ramachandran( load_ramachandran( system.file( "extdata/ramachandran_example.txt.gz",
                                              package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{ramachandran}
\keyword{angles}
\keyword{dihedral}
