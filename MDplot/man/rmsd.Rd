% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{rmsd}
\alias{rmsd}
\title{Root-mean-square-deviation (RMSD) plot}
\description{Plot (multiple) RMSD file(s) as produced by molecular dynamics packages.}
\usage{
rmsd( rmsdData,
      printLegend = TRUE,
      snapshotsPerTimeInt = 1000,
      timeUnit = "ns",
      rmsdUnit = "nm",
      colours = NA,
      names = NA,
      legendPosition = "bottomright",
      barePlot = FALSE,
      ... )}
\arguments{
  \item{rmsdData}{List of (alterating) indices and RMSD values, as produced e.g. by \code{\link{load_rmsd}()}.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{snapshotsPerTimeInt}{Number, specifying how many snapshots are comprising one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{rmsdUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{names}{Vector of the names of the trajectories.}
  \item{legendPosition}{Indicates position of legend: either "\code{bottomright}", "\code{bottomleft}", "\code{topleft}" or "\code{topright}".}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a list of lists, where each sub-list represents a RMSD curve and contains:
\item{minValue}{The minimum value over the whole time range.}
\item{maxValue}{The maximum value over the whole time range.}
\item{meanValue}{The mean value calculated over the whole time range.}
\item{sd}{The standard deviation calculated over the whole time range.}}
\examples{
# GROMOS (see load_rmsd() for other input possibilities)
rmsd( load_rmsd( c( system.file( "extdata/rmsd1_example.txt.gz", package = "MDplot" ),
                    system.file( "extdata/rmsd2_example.txt.gz", package = "MDplot" ) ) ) )
}
\author{Christian Margreitter}
\keyword{Root-mean-square-deviation}