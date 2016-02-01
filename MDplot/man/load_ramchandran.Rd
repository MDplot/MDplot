%  Copyright (C) 2015 Christian Margreitter
\name{load_ramachandran}
\alias{load_ramachandran}
\title{Load dihedral information (Ramachandran plot input)}
\description{
Loads textfile with two columns of dihedral angles, which are to be stored in a matrix. May contain a third column for heat information. By default, the first column is phi and the second psi. Angles can be shifted by a constant value (in order to transform them from 0 to 360 to the usually used -180 to 180).}
\usage{
load_ramachandran( path,
                   angleColumns = c( 1, 2 ),
                   heatColumn = NA,
                   shiftAngles = NA,
                   mdEngine = "GROMOS" )}
\arguments{
\item{path}{Path to input file. At least two columns of the same length are expected.}
\item{angleColumns}{If more columns are present, the angle columns can be chosen by this vector.}
\item{heatColumn}{The (optional) heat column can be specified here.}
\item{shiftAngle}{In order to shift the values by a constant factor (e.g. -180).}
\item{mdEngine}{Argument introduced for distinction between input formats based on the used molecular dynamics engine (to be implemented).}
}
\examples{
# load table and return it into plotting function "MDplot_ramachandran()"
# see "extdata/ramachandran_example.txt" for format information
MDplot::load_ramachandran( system.file( "extdata/ramachandran_example.txt", package = "MDplot" ) )
}
\author{Christian Margreitter}
\keyword{Ramachandran}