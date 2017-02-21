% (C) 2015 Christian Margreitter
% last update: 2017-02-21
\name{translate_aminoacids}
\alias{translate_aminoacids}
\title{Function to translate between canonical and GROMOS amino acid abbreviations}
\description{
Converts an aminoacid naming scheme in the context of GROMOS (e.g. containing HISH) into canonical three- or one-letter codes.}
\usage{
translate_aminoacids( input,
                      switchMode )
}
\arguments{
\item{input}{Vector of GROMOS abbreviations.}
\item{switchMode}{Type "1" results in single-letter, type "2" in three-letter code.}
}
\examples{
translate_aminoacids( c( "ALA", "HISA", "SER" ),
                      switchMode = 2 )
}
\author{Christian Margreitter}
\keyword{aminoacids}
