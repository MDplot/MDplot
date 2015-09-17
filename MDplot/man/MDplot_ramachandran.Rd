\name{auto}
\alias{auto}
\docType{data}
\title{
  Automobile Customer Survey Data
}
\description{
  A German manufacturer of premium cars asked customers approximately 3
  months after a car purchase which characteristics of the car were most
  important for the decision to buy the car. The survey was done in 1983
  and the data set contains all responses without missing values.
}
\usage{data(auto)}
\format{
  A data frame with 793 observations on the following 46 variables.
  \describe{
    \item{\code{model}}{a factor with levels \code{A} \code{B} \code{C}
      \code{D}, model bought by the customer.}
    \item{\code{gear}}{a factor with levels \code{4 gears}, \code{5
	econo}, \code{5 sport}, or \code{automatic}.}
    \item{\code{leasing}}{a logical vector, was leasing used to finance
      the car?}
    \item{\code{usage}}{a factor with levels \code{private}, \code{both}, \code{business}.}
    \item{\code{previous_model}}{a factor describing which type of car
      was owned directly before the purchase.}
    \item{\code{other_consider}}{a factor with levels \code{same manuf},
      \code{other manuf}, \code{both}, or \code{none}}
    \item{\code{test_drive}}{a logical vector, did you do a test drive?}
    \item{\code{info_adv}}{a logical vector, was advertising an
      important source of information?}
    \item{\code{info_exp}}{a logical vector, was experience an
      important source of information?}
    \item{\code{info_rec}}{a logical vector, were recommendations an
      important source of information?}
    \item{\code{ch_clarity}}{a logical vector}
    \item{\code{ch_economy}}{a logical vector}
    \item{\code{ch_driving_properties}}{a logical vector}
    \item{\code{ch_service}}{a logical vector}
    \item{\code{ch_interior}}{a logical vector}
    \item{\code{ch_quality}}{a logical vector}
    \item{\code{ch_technology}}{a logical vector}
    \item{\code{ch_model_continuity}}{a logical vector}
    \item{\code{ch_comfort}}{a logical vector}
    \item{\code{ch_reliability}}{a logical vector}
    \item{\code{ch_handling}}{a logical vector}
    \item{\code{ch_reputation}}{a logical vector}
    \item{\code{ch_concept}}{a logical vector}
    \item{\code{ch_character}}{a logical vector}
    \item{\code{ch_power}}{a logical vector}
    \item{\code{ch_resale_value}}{a logical vector}
    \item{\code{ch_styling}}{a logical vector}
    \item{\code{ch_safety}}{a logical vector}
    \item{\code{ch_sporty}}{a logical vector}
    \item{\code{ch_consumption}}{a logical vector}
    \item{\code{ch_space}}{a logical vector}
    \item{\code{satisfaction}}{a numeric vector describing overall
      satisfaction (1=very good, 10=very bad).}
    \item{\code{good1}}{conception, styling, dimensions.}
    \item{\code{good2}}{auto body.}
    \item{\code{good3}}{driving and coupled axles.}
    \item{\code{good4}}{engine.}
    \item{\code{good5}}{electronics.}
    \item{\code{good6}}{financing and customer service.}
    \item{\code{good7}}{other.}
    \item{\code{sporty}}{What do you think about the balance of
      sportiness and comfort? (\code{good} \code{more sport} \code{more comfort}).}
    \item{\code{drive_char}}{driving characteristis (\code{gentle} < \code{speedy} < \code{powerfull} < \code{extreme}).}
    \item{\code{tempo}}{Which average speed do you prefer on German
      Autobahn in km/h? (\code{< 130} < \code{130-150} < \code{150-180} < \code{> 180})}
    \item{\code{consumption}}{an ordered factor with levels \code{low} < \code{ok} < \code{high} < \code{too high}.}
    \item{\code{gender}}{a factor with levels \code{male} \code{female}}
    \item{\code{occupation}}{a factor with levels \code{self-employed},
      \code{freelance}, and \code{employee}.}
    \item{\code{household}}{size of household, an ordered factor with levels \code{1-2} < \code{>=3}}
  }
}
\source{
  The original German data are in the public domain and available at
  \url{http://dx.doi.org/10.5282/ubm/data.14} from LMU Munich. The
  variable names and help page were translated to English and converted
  into Rd format by Friedrich Leisch.
}
\references{
  Open Data LMU (1983): Umfrage unter Kunden einer Automobilfirma,
  doi:10.5282/ubm/data.14
}
\examples{
data(auto)
summary(auto)
}
\keyword{datasets}
