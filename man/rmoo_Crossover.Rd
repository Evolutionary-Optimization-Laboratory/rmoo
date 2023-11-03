\name{rmoo_Crossover}
\alias{rmoo_Crossover}
%
\alias{rmoo_spCrossover}
%
\alias{rmoobin_spCrossover}
%
\alias{rmooreal_spCrossover}
\alias{rmooreal_sbxCrossover}
%
\alias{rmooperm_oxCrossover}

\title{Crossover operators in non-dominated genetic algorithms}

\description{Functions implementing crossover non-dominated genetic operator.}

\usage{
  rmoo_spCrossover(object, parents)

  rmoobin_spCrossover(object, parents)

  rmooreal_spCrossover(object, parents)
  rmooreal_sbxCrossover(object, parents, nc = 20)

  rmooperm_oxCrossover(object, parents)
}

\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} and \code{"nsga3"}, usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}} and \code{\link{nsga3}}.}

  \item{parents}{A two-rows matrix of values indexing the parents from the current population.}

  \item{nc}{Parameters of non-dominated genetic operators.}
}

\value{
  Return a list with two elements:

    \item{children}{a matrix of dimension 2 times the number of decision variables containing the generated offsprings;}

    \item{fitness}{a vector of length 2 containing the fitness values for the offsprings. A value \code{NA} is returned if an offspring is different (which is usually the case) from the two parents.}
}

\references{
  Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206, doi: 10.32614/RJ-2017-008.
}

\author{Francisco Benitez}

\seealso{
  \code{\link[=nsga]{nsga()}},
  \code{\link[=nsga2]{nsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}

