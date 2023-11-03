\name{rmoo_Population}
\alias{rmoo_Population}
%
\alias{rmoobin_Population}
%
\alias{rmooreal_Population}
%
\alias{rmooperm_Population}

\title{Population initialization in non-dominated genetic algorithms}

\description{Functions for creating a random initial population to be used in non-dominated genetic algorithms.}

\usage{
  rmoobin_Population(object)

  rmooreal_Population(object)

  rmooperm_Population(object)
}

\arguments{

  \item{object}{An object of class \code{\link{nsga-class}}, \code{\link{nsga2-class}} or \code{\link{nsga3-class}}.}

}

\details{
  \code{rmoobin_Population} generates a random population of \code{object@nBits} binary values;

  \code{rmooreal_Population} generates a random (uniform) population of real values in the range [\code{object@lower}, \code{object@upper}];

  \code{rmooperm_Population} generates a random (uniform) population of integer values in the range [\code{object@lower}, \code{object@upper}].
}

\value{Return a matrix of dimension \code{object@popSize} times the number of decision variables.}

\references{
  Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206, doi: 10.32614/RJ-2017-008.
}

\author{Francisco Benitez}

\seealso{
  \code{\link{nsga}},
  \code{\link{nsga2}} and
  \code{\link{nsga3}}
}
