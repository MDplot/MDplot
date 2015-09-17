%
%  Copyright (C) 2005 Friedrich Leisch
%  $Id: flexclustControl-class.Rd 3 2013-06-12 10:06:43Z leisch $
%
\name{flexclustControl-class}
\docType{class}
\alias{cclustControl}
\alias{flexclustControl}
\alias{flexclustControl-class}
\alias{coerce,list,flexclustControl-method}
\alias{coerce,NULL,flexclustControl-method}
\alias{cclustControl-class}
\alias{coerce,list,cclustControl-method}
\alias{coerce,NULL,cclustControl-method}
\title{Classes "flexclustControl" and "cclustControl"}
\description{Hyperparameters for cluster algorithms.}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("flexclustControl", ...)}. In addition, named lists can be
  coerced to \code{flexclustControl}
  objects, names are completed if unique (see examples).
}

\section{Slots}{
  Objects of class \code{flexclustControl} have the following slots:
  \describe{
    \item{\code{iter.max}:}{Maximum number of iterations.}

    \item{\code{tolerance}:}{The algorithm is stopped when the
      (relative) change of the optimization criterion is smaller than
      \code{tolerance}.}
    
    \item{\code{verbose}:}{If a positive integer, then progress is
      reported every \code{verbose} iterations. If 0,
      no output is generated during model fitting.}

    \item{\code{classify}:}{Character string, one of \code{"auto"},
      \code{"weighted"}, \code{"hard"} or \code{"simann"}.}
    
    \item{\code{initcent}:}{Character string, name of function for
      initial centroids, currently \code{"randomcent"} (the default) and
      \code{"kmeanspp"} are available.}
    
    \item{\code{gamma}:}{Gamma value for weighted hard competitive
      learning.}
    \item{\code{simann}:}{Parameters for simulated annealing
      optimization (only used when \code{classify="simann"}).}
    
    \item{\code{ntry}:}{Number of trials per iteration for QT clustering.}
    \item{\code{min.size}:}{Clusters smaller than this value are treated
      as outliers.}
  }
  Objects of class \code{cclustControl} inherit from
  \code{flexclustControl} and have the following additional slots:
  \describe{
    \item{\code{method}:}{Learning rate for hard competitive learning,
      one of \code{"polynomial"} or \code{"exponential"}.}
    \item{\code{pol.rate}:}{Positive number for polynomial learning rate
      of form \eqn{1/iter^par}.}
    \item{\code{exp.rate}}{Vector of length 2 with parameters for
      exponential learning rate of form
      \eqn{par1*(par2/par1)^(iter/iter.max)}}.
    \item{\code{ng.rate}:}{Vector of length 4 with parameters for neural
      gas, see details below.}
  }
}
\section{Learning Rate of Neural Gas}{
  The neural gas algorithm uses updates of form
  \deqn{cnew = cold + e*exp(-m/l)*(x - cold)}
  for every centroid, where \eqn{m} is the order (minus 1) of the
  centroid with
  respect to distance to data point \eqn{x} (0=closest, 1=second,
  \ldots). The parameters \eqn{e} and \eqn{l} are given by
  \deqn{e = par1*(par2/par1)^(iter/iter.max),}
  \deqn{l = par3*(par4/par3)^(iter/iter.max).}
  See Martinetz et al (1993) for details of the algorithm, and the
  examples section on how to obtain default values.
}
\references{
  Martinetz T., Berkovich S., and Schulten K. (1993). "Neural-Gas
  Network for Vector Quantization and its Application to Time-Series
  Prediction." IEEE Transactions on Neural Networks, 4 (4), pp. 558--569.

  Arthur D. and Vassilvitskii S. (2007). "k-means++: the advantages of
  careful seeding". Proceedings of the 18th annual ACM-SIAM symposium on
  Discrete algorithms. pp. 1027-1035.
}
\author{Friedrich Leisch}
\seealso{
  \code{\link{kcca}}, \code{\link{cclust}}
}
\keyword{classes}
\examples{
## have a look at the defaults
new("flexclustControl")

## corce a list
mycont = list(iter=500, tol=0.001, class="w")
as(mycont, "flexclustControl")

## some additional slots
as(mycont, "cclustControl")

## default values for ng.rate
new("cclustControl")@ng.rate
}
