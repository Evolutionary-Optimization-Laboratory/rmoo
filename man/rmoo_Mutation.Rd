\name{rmoo_Mutation}
\alias{rmoo_Mutation}
%
\alias{rmoobin_raMutation}
%
\alias{rmooreal_raMutation}
\alias{rmooreal_polMutation}
%
\alias{rmooperm_simMutation}

\title{Mutation operators in non-dominated genetic algorithms}

\description{Functions implementing mutation non-dominated genetic operator.}

\usage{
  rmoobin_raMutation(object, parent)

  rmooreal_raMutation(object, parent)
  rmooreal_polMutation(object, parent, nm = 0.20, indpb = 0.2)

  rmooperm_simMutation(object, parent)
}

\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"} usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}}, \code{\link{nsga3}}.}

  \item{parent}{A vector of values for the parent from the current population where mutation should occur.}

  \item{nm}{Parameters of genetic operators.}

  \item{indpb}{Parameters.}
}

\value{Return a vector of values containing the mutated string.}

\references{
  Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206, doi: 10.32614/RJ-2017-008.
}

\author{Francisco Benitez}
