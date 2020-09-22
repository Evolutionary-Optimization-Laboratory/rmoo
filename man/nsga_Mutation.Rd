\name{nsga_Mutation}
\alias{nsga_Mutation}
%
\alias{nsgabin_raMutation}
%
\alias{nsgareal_raMutation}
\alias{nsgareal_polMutation}
%
\alias{nsgaperm_simMutation}

\title{Mutation operators in non-dominated genetic algorithms}

\description{Functions implementing mutation non-dominated genetic operator.}

\usage{
  nsgabin_raMutation(object, parent)
  
  nsgareal_raMutation(object, parent)
  nsgareal_polMutation(object, parent, nm = 0.20)
  
  nsgaperm_simMutation(object, parent)
}

\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"} usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}}, \code{\link{nsga3}}.}
  
  \item{parent}{A vector of values for the parent from the current population where mutation should occur.}
  
  \item{nm}{Parameters of genetic operators.}
}

\value{Return a vector of values containing the mutated string.}

\references{
  Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
  \url{http://www.jstatsoft.org/v53/i04/}.
}

\author{Francisco Benitez}
