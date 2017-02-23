% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{dssp}
\alias{dssp}
\title{DSSP plot for secondary structure elements (proteins)}
\description{
Plots summary plot for secondary structure motifs based on the output of the widely used classification program DSSP, which uses hydrogen bonds for classification. The default order is: "3-Helix", "4-Helix", "5-Helix", "Bend", "Beta-Bridge", "Beta-Strand", "Turn" (depending on the input, not all types might be included).}
\usage{
dssp( dsspData,
      printLegend = FALSE,
      useOwnLegend = FALSE,
      elementNames = NA,
      colours = NA,
      showValues = NA,
      showResidues = NA,
      plotType = "dots",
      selectedElements = NA,
      barePlot = FALSE,
      ... )}
\arguments{
  \item{dsspData}{Table containing the information on the secondary structure motifs.}
  \item{printLegend}{If \code{TRUE}, a legend is printed on the right hand side of the plot.}
  \item{useOwnLegend}{If \code{FALSE}, the names of the secondary structure elements are considered to be in default order.}
  \item{elementNames}{Vector of names for the secondary structure elements.}
  \item{colours}{A vector of colours, that can be specified to replace the default ones.}
  \item{showValues}{A vector of boundaries for the values (two elements).}
  \item{showResidues}{A vector of boundaries for the residues (two elements).}
  \item{plotType}{Either "\code{dots}", "\code{curves}" or "\code{bars}".}
  \item{selectedElements}{A vector of names of the elements selected for plotting.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns matrix, where the first column is the residue-number and the remaining ones denote secondary structure classes. Residues are given row-wise and values range from \code{0} to \code{100} percent.
}
\examples{
# GROMOS (see load_dssp() for other input possibilities)
dssp( load_dssp( system.file( "extdata/dssp_example.txt.gz",
                 package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{dssp}