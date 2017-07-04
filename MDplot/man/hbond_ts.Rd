% (C) 2015 Christian Margreitter
% last update: 2017-07-04
\name{hbond_ts}
\alias{hbond_ts}
\title{Plot hydrogen bond timeseries}
\description{Timeseries plot of hydrogen bonds (various selections possible). In addition to the timeseries file, depending on the MD engine format used, an additional summary file might also be necessary (see below for examples).}
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
\item{timeseries}{Table, containing the timeseries information (e.g. produced by \code{\link{load_hbond_ts}()}).}
\item{summary}{Table, containing the summary information (e.g. produced by \code{\link{load_hbond}()}).}
\item{acceptorRange}{Vector of acceptor resdiues, which are to be plotted.}
\item{donorRange}{Vector of donor residues, which are to be plotted.}
\item{plotOccurences}{Specifies, whether the overall summary should also be plotted in a subplot on the right hand side.}
\item{scalingFactorPlot}{To manually set the scaling factor (if necessary).}
\item{printNames}{Enables human readable names rather than the hbond identifiers.}
\item{namesToSingle}{If \code{printNames} is \code{TRUE}, this flags enables one letter codes.}
\item{printAtoms}{If \code{printNames} is \code{TRUE}, this flag adds the atom names in the hydrogen bond identification on the y-axis.}
\item{timeUnit}{Specifies the time unit on the x-axis.}
\item{snapshotsPerTimeInt}{Specifies, how many snapshots make up one time unit (see above).}
\item{timeRange}{Selects a certain time range specified by a vector.}
\item{hbondIndices}{List, containing vectors to select hbonds by their identifiers.}
\item{barePlot}{Boolean, indicating whether the plot is to be made without any additional information.}
\item{...}{Additional arguments (ellipsis).}
}
\value{
Returns a \code{n}x2 matrix, with the first column being the list of hydrogen bond identifiers plotted and the second one the occurrence (in percent) over the selected time range.
}
\examples{
# the examples are valid and wrapped in the "dontrun{}" environment for efficiency purposes only
# GROMOS
\dontrun{
hbond_ts( load_hbond_ts( system.file( "extdata/hbond_ts_example.txt.gz",
                                      package = "MDplot" ) ),
          load_hbond( system.file( "extdata/hbond_example.txt.gz",
                                   package = "MDplot" ) ) )}

# GROMACS
\dontrun{
hbond_ts( load_hbond_ts( system.file( "extdata/hbond_ts_example_GROMACS.xpm.gz",
                                      package = "MDplot" ),
                         mdEngine = "GROMACS" ),
          load_hbond( system.file( "extdata/hbond_ts_example_GROMACS.xpm.gz",
                                  package = "MDplot" ),
                      system.file( "extdata/hbond_example_GROMACS.txt.gz",
                                   package = "MDplot" ),
                      mdEngine = "GROMACS" ),
          plotOccurences = TRUE, namesToSingle = FALSE, printNames = TRUE,
          printAtoms = TRUE, hbondIndices = list( c( 1, 12 ) ),
          timeUnit = "ns", snapshotsPerTimeInt = 100 )}

# AMBER
hbond_ts( load_hbond_ts( system.file( "extdata/hbond_ts_example_AMBER.txt.gz",
                                      package = "MDplot" ),
                         mdEngine = "AMBER" ),
          load_hbond( system.file( "extdata/hbond_ts_example_AMBER.txt.gz",
                                   package = "MDplot" ),
                         mdEngine = "AMBER" ),
          plotOccurences = TRUE, timeRange = c( 20, 60 ) )
}
\author{Christian Margreitter}
\keyword{hbond}
\keyword{hydrogen bond}
\keyword{timeseries}
