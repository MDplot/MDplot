%  Copyright (C) 2015 Christian Margreitter
\name{load_xrmsd}
\alias{load_xrmsd}
\title{Loading function for MDplot::xrmsd()}
\description{
Loads matrix information from the specified file. Caution: the function is in beta-state and extremely sensitive to format changes.}
\usage{
load_xrmsd(  path,
             factor = 10000,
             mdEngine = "GROMOS"  )
}
\arguments{
  \item{path}{Specifies the input file.}
  \item{factor}{In case the RMSD values are given in \deqn{nm * factor}, the factor can be specified. If the unit is already nanometers, 1 is the appropriate value.}
  \item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# loading
load_xrmsd( system.file( "extdata/xrmsd_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{xrmsd}
