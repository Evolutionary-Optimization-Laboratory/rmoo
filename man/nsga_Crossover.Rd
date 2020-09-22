\name{nsga_Crossover}
\alias{nsga_Crossover}
%
\alias{nsga_spCrossover}
%
\alias{nsgabin_spCrossover}
%
\alias{nsgareal_spCrossover}
\alias{nsgareal_sbxCrossover}
%
\alias{nsgaperm_oxCrossover}

\title{Crossover operators in non-dominated genetic algorithms}

\description{Functions implementing crossover non-dominated genetic operator.}

\usage{
  nsga_spCrossover(object, parents)
  
  nsgabin_spCrossover(object, parents)
  
  nsgareal_spCrossover(object, parents)
  nsgareal_sbxCrossover(object, parents, nc = 20)
  
  nsgaperm_oxCrossover(object, parents)
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
  Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
  \url{http://www.jstatsoft.org/v53/i04/}.
}

\author{Francisco Benitez}

\seealso{
  \code{\link[=nsga]{nsga()}},
  \code{\link[=nsga2]{nsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}

