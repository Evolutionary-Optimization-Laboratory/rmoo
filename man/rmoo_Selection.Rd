\name{rmoo_Selection}
\alias{rmoo_Selection}
%
%
\alias{rmoo_lrSelection}
\alias{rmoo_tourSelection}
%
\alias{rmoobin_lrSelection}
\alias{rmoobin_tourSelection}
%
\alias{rmooreal_lrSelection}
\alias{rmooreal_tourSelection}
%
\alias{rmooperm_lrSelection}
\alias{rmooperm_tourSelection}

\title{Selection operators in non-dominated genetic algorithms}

\description{Functions implementing selection non-dominated genetic operator.}

\usage{
  rmoo_lrSelection(object, r, q)
  rmoo_tourSelection(object, k = 2, \dots)

  rmoobin_lrSelection(object, r, q)
  rmoobin_tourSelection(object, k = 2, \dots)

  rmooreal_lrSelection(object, r, q)
  rmooreal_tourSelection(object, k = 2, \dots)

  rmooperm_lrSelection(object, r, q)
  rmooperm_tourSelection(object, k = 2, \dots)
}

\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"}, usually resulting from a call to function  \code{\link{nsga}},  \code{\link{nsga2}} or \code{\link{nsga3}}.}

  \item{r}{A tuning parameter for the specific selection operator.}

  \item{q}{A tuning parameter for the specific selection operator.}

  \item{k}{A tuning parameter for the specific selection operator.}

  \item{\dots}{Further arguments passed to or from other methods.}

}

\value{
  Return a list with two elements:

    \item{population}{a matrix of dimension \code{object@popSize} times the number of decision variables containing the selected individuals or strings;}

  \item{fitness}{a vector of length \code{object@popSize} containing the fitness values for the selected individuals.}
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

