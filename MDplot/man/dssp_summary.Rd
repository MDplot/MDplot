%  Copyright (C) 2015 Christian Margreitter
\name{dssp_summary}
\alias{dssp_summary}
\title{DSSP summary plot for secondary structure elements (proteins)}
\description{
Plots summary plot for secondary structure motifs based on the output of the widely used classification program DSSP, which takes hydrogen bonds into account. The default order is: "3-Helix", "4-Helix", "5-Helix", "Bend", "Beta-Bridge", "Beta-Strand", "Turn".}
\usage{
dssp_summary( dsspData,
              printLegend = FALSE,
              useOwnLegend = FALSE,
              motifNames = NA,
              colours = NA,
              showValues = NA,
              showResidues = NA,
              plotType = "dots",
              selectedMotifs = NA,
              barePlot = FALSE,
              ... )}
\arguments{
  \item{dsspData}{Table containing the information on the secondary structure motifs.}
  \item{printLegend}{If \code{TRUE}, a legend is printed on the right hand side of the plot.}
  \item{useOwnLegend}{If \code{FALSE}, the names of the secondary structure motifs are considered to be in default order.}
  \item{motifNames}{Vector of names for the secondary structure motifs.}
  \item{colours}{A vector of colours, that can be specified to replace the default ones.}
  \item{showValues}{A vector of boundaries for the values (two elements).}
  \item{showResidues}{A vector of boundaries for the residues (two elements).}
  \item{plotType}{Either \code{dots}, \code{curves} or \code{bars}.}
  \item{selectedMotifs}{A vector of names of the motifs selected for plotting.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
}
\examples{
# see "extdata/dssp_summary_example.txt" for format information
MDplot::dssp_summary( MDplot::load_dssp_summary( system.file( "extdata/dssp_summary_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{dssp}