\name{nsga_Mutation}
\alias{nsga_Mutation}
%

\alias{nsgareal_polMutation}
\alias{nsgareal_raMutation}

\title{Mutation operators in non-dominated genetic algorithms}

\description{Functions implementing mutation non-dominated genetic operator.}

\usage{

nsgareal_polMutation(object, parent, nm = 0.20)
nsgareal_raMutation(object, parent)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"} usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}}, \code{\link{nsga3}}.}

  \item{parent}{A vector of values for the parent from the current population where mutation should occur.}

  \item{nm}{Parameters of genetic operators.}

  \item{\dots}{Further arguments passed to or from other methods.}
}

\value{Return a vector of values containing the mutated string.}

\author{Francisco Benitez}
