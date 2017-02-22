% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{load_xrmsd}
\alias{load_xrmsd}
\title{Loading function for \code{\link{xrmsd}()}}
\description{
Loads matrix information from the specified file.}
\usage{
load_xrmsd(  path,
             factor = 1,
             removeLowerHalf = TRUE,
             mdEngine = "GROMOS"  )
}
\arguments{
  \item{path}{Specifies the input file.}
  \item{factor}{In case the RMSD values are given in \deqn{nm * factor}, the factor can be specified. If the unit is already nanometers, 1 is the appropriate value.}
  \item{removeLowerHalf}{If \code{TRUE}, the lower half of the plot will be white.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
Returns a \code{n}x3-matrix, with the first two columns holding the position (x- and y-axis) and the third the respective RMSD value.
}
\examples{
# GROMOS
load_xrmsd( system.file( "extdata/xrmsd_example.txt.gz",
                         package = "MDplot" ),
            factor = 10000 )

# GROMACS
load_xrmsd( system.file( "extdata/xrmsd_example_GROMACS.xpm.gz",
                         package = "MDplot" ),
            mdEngine = "GROMACS" )

# AMBER
load_xrmsd( system.file( "extdata/xrmsd_example_AMBER.txt.gz",
                         package = "MDplot" ),
            mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{xrmsd}
