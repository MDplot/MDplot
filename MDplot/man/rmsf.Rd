% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{rmsf}
\alias{rmsf}
\title{Root-mean-square-fluctuation (RMSF) plot}
\description{Plot (multiple) RMSF file(s) as produced by molecular dynamics packages.}
\usage{
rmsf( rmsfData,
      printLegend = TRUE,
      rmsfUnit = "nm",
      colours = NA,
      residuewise = FALSE,
      atomsPerResidue = NA,
      names = NA,
      rangeAtoms = NA,
      legendPosition = "topright",
      barePlot = FALSE,
      ... )}
\arguments{
  \item{rmsfData}{List of (alterating) indices and RMSF values, as produced e.g. by \code{\link{load_rmsf}()}.}
  \item{printLegend}{Boolean, which triggers plotting of the legend.}
  \item{rmsfUnit}{Specifies, which unit the y-axis is given in.}
  \item{colours}{Vector of colours used for plotting.}
  \item{residuewise}{Boolean, specifying whether atoms or residues are plotted on the x-axis.}
  \item{atomsPerResidue}{If \code{residuewise} is \code{TRUE}, this parameter can be used to specify the number of atoms per residue for plotting.}
  \item{names}{Vector of the names of the trajectories.}
  \item{rangeAtoms}{Range of atoms to be plotted.}
  \item{legendPosition}{Indicate position of legend: either "\code{bottomright}", "\code{bottomleft}", "\code{topleft}" or "\code{topright}".}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
A list of vectors, alternately holding atom indices and their respective values.
}
\examples{
# GROMOS (see load_rmsf() for other input possibilities)
rmsf( load_rmsf( c( system.file( "extdata/rmsf_example_1.txt.gz", package = "MDplot" ),
                    system.file( "extdata/rmsf_example_2.txt.gz", package = "MDplot" ) ) ) )
}
\author{Christian Margreitter}
\keyword{Root-mean-square-fluctuation}
