% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{load_ramachandran}
\alias{load_ramachandran}
\title{Load dihedral information (Ramachandran plot input)}
\description{
Loads a textfile with dihedral angles, which are to be stored in a matrix. By default, the first column is phi and the second psi. Angles can be shifted by a constant value (in order to transform them from 0 to 360 to the usually used -180 to 180).}
\usage{
load_ramachandran( path,
                   angleColumns = c(1,2),
                   shiftAngles = NA,
                   mdEngine = "GROMOS" )}
\arguments{
\item{path}{Path to input file. At least two columns of the same length are expected.}
\item{angleColumns}{If more columns are present, the angle columns can be chosen by this vector.}
\item{shiftAngles}{In order to shift the values by a constant factor (e.g. -180).}
\item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}.}
}
\value{
A \code{n}x2-matrix with phi and psi angles in the respective columns.
}
\examples{
# GROMOS
load_ramachandran( system.file( "extdata/ramachandran_example.txt.gz", package = "MDplot" ) )

# GROMACS
load_ramachandran( system.file( "extdata/ramachandran_example_GROMACS.txt.gz",
                   package = "MDplot" ), mdEngine = "GROMACS" )

# AMBER
load_ramachandran( system.file( "extdata/ramachandran_example_AMBER.txt.gz",
                   package = "MDplot" ), mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{Ramachandran}
