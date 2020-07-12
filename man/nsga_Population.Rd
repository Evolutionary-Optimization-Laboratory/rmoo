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
nsgabin_Population(object, ...)

nsgareal_Population(object, ...)

nsgaperm_Population(object, ...)
}

\arguments{

  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"} usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}} or \code{\link{nsga3}}.}

  \item{\dots}{Further arguments passed to or from other methods.}
}

\details{
\code{nsgabin_Population} generates a random population of \code{object@nBits} binary values;

\code{nsgareal_Population} generates a random (uniform) population of real values in the range [\code{object@lower}, \code{object@upper}];

\code{gaperm_Population} generates a random (uniform) population of integer values in the range [\code{object@lower}, \code{object@upper}].
}

\value{Return a matrix of dimension \code{object@popSize} times the number of decision variables.}

%\references{}

\author{Francisco Benitez}

%\note{}

\seealso{
  \code{\link[=nsga]{nsga()}},
  \code{\link[=nsga2]{nsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}
