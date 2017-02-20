% (C) 2015 Christian Margreitter
% last update: 2017-02-20
\name{load_clusters}
\alias{load_clusters}
\title{Loading cluster information}
\description{
This function loads clusters from a plain text file and stores them in a matrix. The trajectories can be named by the user. The output of this function can be used as input of function \code{\link{clusters}()}.}
\usage{
load_clusters( path,
               names = NA,
               mdEngine = "GROMOS" )
}
\arguments{
  \item{path}{Specifies the path of the text input file.}
  \item{names}{Optional vector of trajectory names. If provided, needs to be of the same length as the number of clusters to be plotted.}
  \item{mdEngine}{Argument distinguishes between input formats based on the molecular dynamics engine used. Currently available: \code{"GROMOS"}, \code{"GROMACS"} and \code{"AMBER"}. Note, that two different kinds of \code{AMBER} output may be loaded (see example input files).}
}
\value{
Returns a \code{n}x\code{m}-matrix with \code{n} being the number of input trajectories and \code{m} the number of different clusters. Each element in the matrix holds the number of snapshots, the respective cluster occured in the respective trajectory. 
}
\examples{
# GROMOS
load_clusters( system.file( "extdata/clusters_example.txt.gz", package = "MDplot" ) )

# GROMACS
load_clusters( system.file( "extdata/clusters_example_GROMACS.txt.gz", package = "MDplot" ),
               mdEngine = "GROMACS", lengths = c( 1001, 1001 ) )

# AMBER (first)
load_clusters( system.file( "extdata/clusters_example_1_AMBER.txt.gz", package = "MDplot" ),
               mdEngine = "AMBER" )

# AMBER (second)
load_clusters( system.file( "extdata/clusters_example_2_AMBER.txt.gz", package = "MDplot" ),
               mdEngine = "AMBER" )
}
\author{Christian Margreitter}
\keyword{Clusters}
