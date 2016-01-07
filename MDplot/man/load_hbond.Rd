%  Copyright (C) 2015 Christian Margreitter
\name{MDplot_load_hbond}
\alias{MDplot_load_hbond}
\title{DSSP summary plot for secondary structure elements (proteins)}
\description{
Plots summary plot for secondary structure motifs based on the output of the widely used classification program DSSP, which takes hydrogen bonds into account. The defaul order is: "3-Helix", "4-Helix", "5-Helix", "Bend", "Beta-Bridge", "Beta-Strand", "Turn".}
\usage{
MDplot_DSSP_summary( TABLE_datainput,
                     BOOL_printLegend = FALSE,
                     BOOL_useOwnLegend = FALSE,
                     COLOURS_DSSP_summary = NULL,
                     VEC_showValues = NULL,
                     VEC_showResidues = NULL,
                     ... )}
\arguments{
  \item{TABLE_datainput}{Table containing the information on the hydrogen bonds.}
  \item{BOOL_printLegend}{If "TRUE", a legend is printed on the right hand side of the plot.}
  \item{BOOL_useOwnLegend}{If "FALSE", the names of the secondary structure motifs are guessed from the common order.}
  \item{COLOURS_DSSP_summary}{A vector of colours, that can be specified to replace the default ones.}
  \item{VEC_showValues}{A vector of boundaries for the values (two elements).}
  \item{VEC_showResidues}{A vector of boundaries for the residues (two elements).}
  \item{...}{Graphical parameters.}
}
\examples{
# load table and return it into plotting function "MDplot_DSSP_summary()"
# see "extdata/MDplot_DSSP_summary_example.txt" for format information
MDplot_DSSP_summary( MDplot_load_DSSP_summary( system.file( "extdata/MDplot_DSSP_summary_example.txt", package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{DSSP}