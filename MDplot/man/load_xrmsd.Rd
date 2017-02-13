%  Copyright (C) 2015 Christian Margreitter
\name{load_xrmsd}
\alias{load_xrmsd}
\title{Loading function for \code{\link{xrmsd}()}}
\description{
Loads matrix information from the specified file.}
\usage{
load_xrmsd(  path,
             factor = 10000,
             removeLowerHalf = TRUE,
             mdEngine = "GROMOS"  )
}
\arguments{
  \item{path}{Specifies the input file.}
  \item{factor}{In case the RMSD values are given in \deqn{nm * factor}, the factor can be specified. If the unit is already nanometers, 1 is the appropriate value.}
  \item{removeLowerHalf}{If \code{TRUE}, the lower half of the plot will be white.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# loading
# GROMOS
load_xrmsd( system.file( "extdata/xrmsd_example.txt.gz", package = "MDplot" ) )

# GROMACS
load_xrmsd( system.file( "extdata/xrmsd_example_GROMACS.txt.gz",
                         package = "MDplot" ),
            mdEngine = "GROMACS" )
}
\author{Christian Margreitter}
\keyword{xrmsd}
