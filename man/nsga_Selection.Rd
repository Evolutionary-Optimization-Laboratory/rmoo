\name{nsga_Selection}
\alias{nsga_Selection}
%
\alias{nsgareal_tourSelection}

\title{Selection operators in non-dominated genetic algorithms}

\description{Functions implementing selection non-dominated genetic operator.}

\usage{
nsgareal_tourSelection(object, k = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{"nsga"}, \code{"nsga2"} or \code{"nsga3"}, usually resulting from a call to function  \code{\link{nsga}},  \code{\link{nsga2}} or \code{\link{nsga3}}.}

  \item{r}{A tuning parameter for the specific selection operator.}

  \item{\dots}{Further arguments passed to or from other methods.}

}

%\details{}

\value{
Return a list with two elements:

\item{population}{a matrix of dimension \code{object@popSize} times the number of decision variables containing the selected individuals or strings;}

\item{fitness}{a vector of length \code{object@popSize} containing the fitness values for the selected individuals.}
}

\author{Francisco Benitez}


\seealso{
  \code{\link[=nsga]{nsga()}},
  \code{\link[=nsga2]{nsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}

