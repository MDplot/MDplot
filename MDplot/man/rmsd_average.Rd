% (C) 2017 Christian Margreitter
% last update: 2017-02-20
\name{rmsd_average}
\alias{rmsd_average}
\title{Root-mean-square-deviation (RMSD) average plot}
\description{Combines several RMSD index-value pairs and computes and plots the mean value and the spread (the respective minimum and maximum values) at every timepoint. This function is particularly useful, when multiple identical simulation runs (replicates) need to be analysed since it shows a 'corridor' which allows interpretation e.g. of the overall stability.}
\usage{
rmsd_average( rmsdInput,
              skip = 0,
              printMeans = FALSE,
              levelFactor = NA,
              snapshotsPerTimeInt = 1000,
              timeUnit = "ns",
              rmsdUnit = "nm",
              maxYAxis = NA,
              barePlot = FALSE,
              ... )}
\arguments{
  \item{rmsdInput}{}
  \item{skip}{}
  \item{printMeans}{}
  \item{levelFactor}{}
  \item{snapshotsPerTimeInt}{Number, specifying how many snapshots are comprising one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{rmsdUnit}{Specifies, which unit the y-axis is given in.}
  \item{maxYAxis}{}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
}
\examples{
# GROMOS (see load_rmsd() for other input possibilities)
rmsd_average( load_rmsd( c( system.file( "extdata/rmsd1_example.txt.gz", package = "MDplot" ),
                    system.file( "extdata/rmsd2_example.txt.gz", package = "MDplot" ) ) ) )
}
\author{Christian Margreitter}
\keyword{Root-mean-square-deviation}