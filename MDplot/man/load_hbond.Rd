% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{load_hbond}
\alias{load_hbond}
\title{Loading hydrogen bond data}
\description{
This function loads hydrogen bond information from a text file and stores it in a table. See functions \code{\link{hbond}()} and \code{\link{hbond_ts}()} for usage of the return value.}
\usage{
load_hbond( path,
            GROMACShbondlogfile = NA,
            mdEngine = "GROMOS" ) }
\arguments{
  \item{path}{Specifies the path of the text input file.}
  \item{GROMACShbondlogfile}{Additional file path required in case GROMACS format is specified.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}. Note, that \code{load_hbond()} is able to load both summary and time-series data for the AMBER simulation package.}
}
\value{
Returns a table, where the hydrogen bonds are stored in a row-wise fashion and the columns hold the following information (note, that information not available e.g. because the MD analysis tool output does not provide it, is represented by \code{NA} values):
\itemize{\item{\code{hbondID} The identifier number of the hydrogen bonds (if not given by the input, they are numbered ascendingly).}
\item{\code{resDonor} Number of the donor residue.}
\item{\code{resDonorName} Name of the donor residue.}
\item{\code{resAcceptor} Number of the acceptor residue.}
\item{\code{resAcceptorName} Name of the acceptor residue.}
\item{\code{atomDonor} Number of atom, that is the hydrogen bond donor.}
\item{\code{atomDonorName} Name of atom, that is the hydrogen bond donor.}
\item{\code{atomH} Number of atom (proton) that is forming the hydrogen bond.}
\item{\code{atomAcceptor} Number of atom, that is the hydrogen bond acceptor.}
\item{\code{atomAcceptorName} Name of atom, that is the hydrogen bond acceptor.}
\item{\code{percentage} Number between \code{0} and \code{100} in percent representing the occurence rate of a particular hydrogen bond over the trajectory.}
}}
\examples{
# GROMOS
load_hbond( system.file( "extdata/hbond_example.txt.gz", package = "MDplot" ) )

# GROMACS
load_hbond( system.file( "extdata/hbond_ts_example_GROMACS.xpm.gz",
                         package = "MDplot" ),
            system.file( "extdata/hbond_example_GROMACS.txt.gz",
                         package = "MDplot" ),
            mdEngine = "GROMACS" )

# AMBER (summary)
load_hbond( system.file( "extdata/hbond_example_AMBER.txt.gz",
            package = "MDplot" ), mdEngine = "AMBER" )

# AMBER (time-series)
load_hbond( system.file( "extdata/hbond_ts_example_AMBER.txt.gz",
            package = "MDplot" ), mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{hbond}
