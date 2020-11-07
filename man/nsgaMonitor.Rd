\name{nsgaMonitor}
\alias{nsgaMonitor}

\title{Monitor non-dominated genetic algorithm evolution}

\description{Functions to plotting fitness values at each iteration of a search for the 'rmoo' package.}

\usage{
  nsgaMonitor(object, number_objectives, \dots)
}

\arguments{
  \item{object}{an object of class \code{nsga}, \code{nsga2} or \code{nsga3}, usually resulting from a call to function \code{\link{nsga}}, \code{\link{nsga2}} or \code{\link{nsga3}}, respectively.}
  \item{number_objectives}{numbers of objective values of the function to evaluate.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\value{
  These functions plot the fitness values of the current step of the nsga3 on the console. \cr
  By default, \code{nsgaMonitor} is called in interactive sessions by \code{\link{nsga}}, \code{\link{nsga2}}, or \code{\link{nsga3}}. \cr
  The function can be modified by the user to plot or print the values it considers by iteration. \cr
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
