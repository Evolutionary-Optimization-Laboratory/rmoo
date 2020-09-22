\name{nsga_Population}
\alias{nsga_Population}
%
\alias{nsgabin_Population}
%
\alias{nsgareal_Population}
%
\alias{nsgaperm_Population}

\title{Population initialization in non-dominated genetic algorithms}

\description{Functions for creating a random initial population to be used in non-dominated genetic algorithms.}

\usage{
  nsgabin_Population(object)
  
  nsgareal_Population(object)
  
  nsgaperm_Population(object)
}

\arguments{
  
  \item{object}{An object of class \code{\link{nsga-class}}, \code{\link{nsga2-class}} or \code{\link{nsga3-class}}.}
  
}

\details{
  \code{nsgabin_Population} generates a random population of \code{object@nBits} binary values;
  
  \code{nsgareal_Population} generates a random (uniform) population of real values in the range [\code{object@lower}, \code{object@upper}];
  
  \code{nsgaperm_Population} generates a random (uniform) population of integer values in the range [\code{object@lower}, \code{object@upper}].
}

\value{Return a matrix of dimension \code{object@popSize} times the number of decision variables.}

\references{
  Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
  \url{http://www.jstatsoft.org/v53/i04/}.
}

\author{Francisco Benitez}

\seealso{
  \code{\link{nsga}},
  \code{\link{nsga2}} and
  \code{\link{nsga3}}
}
