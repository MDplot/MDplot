%  Copyright (C) 2015 Christian Margreitter
\name{translate_aminoacids}
\alias{translate_aminoacids}
\title{Function to translate between canonical and GROMOS amino acid abbreviations}
\description{
Converts an aminoacid naming scheme in the context of GROMOS (e.g. containing HISH) into canonical three- or one-letter codes.}
\usage{
translate_aminoacids <- function( input,
                                  switchMode)
}
\arguments{
\item{input}{Vector of GROMOS abbreviations.}
\item{switchMode}{Type "1" results in three letter, type "2" in single-letter code.}
}
\examples{
# to one letter code
test <- c( "ALA", "HISA", "SER" )
translate_aminoacids( test, switchMode = 2 )
}
\author{Christian Margreitter}
\keyword{aminoacids}