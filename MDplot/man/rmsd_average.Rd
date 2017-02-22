% (C) 2017 Christian Margreitter
% last update: 2017-02-20
\name{rmsd_average}
\alias{rmsd_average}
\title{Root-mean-square-deviation (RMSD) average plot}
\description{Combines several RMSD index-value pairs and computes and plots the mean value and the spread (the respective minimum and maximum values) at every timepoint. This function is particularly useful, when multiple identical simulation runs (replicates) need to be analysed since it shows a 'corridor' which allows interpretation e.g. of the overall stability.}
\usage{
rmsd_average( rmsdInput,
              levelFactor = NA,
              snapshotsPerTimeInt = 1000,
              timeUnit = "ns",
              rmsdUnit = "nm",
              maxYAxis = NA,
              barePlot = FALSE,
              ... )
}
\arguments{
  \item{rmsdInput}{List of input tables (which are provided by function \code{\link{load_rmsd}()}).}
  \item{levelFactor}{If there are many datapoints, this parameter may be used to use only the \code{levelFactor}th datapoints to obtain a nicer graph.}
  \item{snapshotsPerTimeInt}{Number, specifying how many snapshots are comprising one \code{timeUnit}.}
  \item{timeUnit}{Specifies, which unit the x-axis is given in.}
  \item{rmsdUnit}{Specifies, which unit the y-axis is given in.}
  \item{maxYAxis}{Can be used to manually set the y-axis of the plot.}
  \item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
  \item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a \code{n}x4-matrix, with the rows representing different snapshots and the columns the respective values as follows:
\itemize{\item{snapshot} Index of the snapshot.
\item{minimum} The minimum RMSD value over all input sources at a given time.
\item{mean} The mean RMSD value over all input sources at a given time.
\item{maximum} The maximum RMSD value over all input sources at a given time.
}}
\examples{
# GROMOS (see load_rmsd() for other input possibilities)
rmsd_average( list( load_rmsd( system.file( "extdata/rmsd_example_1.txt.gz",
                                            package = "MDplot" ) ),
                    load_rmsd( system.file( "extdata/rmsd_example_2.txt.gz",
                                            package = "MDplot" ) ) ),
              snapshotsPerTimeInt = 2000, maxYAxis = 0.445 )
}
\author{Christian Margreitter}
\keyword{Root-mean-square-deviation}