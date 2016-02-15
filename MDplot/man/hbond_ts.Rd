%  Copyright (C) 2015 Christian Margreitter
\name{hbond_ts}
\alias{hbond_ts}
\title{Plot hydrogen bond timeseries}
\description{Timeseries plot of hydrogen bonds (various selections possible). In addition to the timeseries file, the summary file is also necessary.}
\usage{
hbond_ts( timeseries,
          summary,
          acceptorRange = NA,
          donorRange = NA,
          plotOccurences = FALSE,
          scalingFactorPlot = NA,
          printNames = FALSE,
          namesToSingle = FALSE,
          printAtoms = FALSE,
          timeUnit = NA,
          snapshotsPerTimeInt = 1000,
          timeRange = NA,
          hbondIndices = NA,
          barePlot = FALSE,
          ... )
}
\arguments{
\item{timeseries}{Table, containing the timeseries information (e.g. produced by MDplot::load_hbond_ts()).}
\item{summary}{Table, containing the summary information (e.g. produced by MDplot::load_hbond()).}
\item{acceptorRange}{Vector of acceptor resdiues, which are to be plotted.}
\item{donorRange}{Vector of donor residues, which are to be plotted.}
\item{plotOccurences}{Specifies, whether the overall summary should also be plotted in a subplot on the right hand side.}
\item{scalingFactorPlot}{To manually set the scaling factor (if necessary).}
\item{printNames}{Enables human readable names rather than the hbond identifiers.}
\item{namesToSingle}{If \code{printNames} is \code{TRUE}, this flags enables one letter codes.}
\item{printAtoms}{Enables atom names in hydrogen bond identification on the y-axis.}
\item{timeUnit}{Specifies the time unit on the x-axis.}
\item{snapshotsPerTimeInt}{Specifies, how many snapshots make up one time unit (see above).}
\item{timeRange}{Selects a certain time range specified by a vector.}
\item{hbondIndices}{List, containing vectors to select hbonds by their identifiers.}
\item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
\item{...}{Additional arguments (ellipsis).}
}
\examples{
# see "extdata/hbond_example.txt.gz" and "extdata/hbond_ts_example.txt.gz" for format information
hbond_ts( load_hbond_ts( system.file( "extdata/hbond_ts_example.txt.gz",
                                      package = "MDplot" ) ),
          load_hbond( system.file( "extdata/hbond_example.txt.gz",
                                   package = "MDplot" ) ) )
}
\author{Christian Margreitter}
\keyword{hbond}
\keyword{hydrogen bond}
\keyword{timeseries}
