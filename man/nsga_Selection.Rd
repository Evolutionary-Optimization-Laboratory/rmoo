\name{nsga_Selection}
\alias{nsga_Selection}
%
%
\alias{nsga_lrSelection}
\alias{nsga_tourSelection}
%
\alias{nsgabin_lrSelection}
\alias{nsgabin_tourSelection}
%
\alias{nsgareal_lrSelection}
\alias{nsgareal_tourSelection}
%
\alias{nsgaperm_lrSelection}
\alias{nsgaperm_tourSelection}

\title{Selection operators in non-dominated genetic algorithms}

\description{Functions implementing selection non-dominated genetic operator.}

\usage{
  nsga_lrSelection(object, r, q)
  nsga_tourSelection(object, k = 3, \dots)
  
  nsgabin_lrSelection(object, r, q)
  nsgabin_tourSelection(object, k = 3, \dots)
  
  nsgareal_lrSelection(object, r, q)
  nsgareal_tourSelection(object, k = 3, \dots)
  
  nsgaperm_lrSelection(object, r, q)
  nsgaperm_tourSelection(object, k = 3, \dots)
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
  Scrucca, L. (2017) On some extensions to GA package: hybrid optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206.
  \url{https://journal.r-project.org/archive/2017/RJ-2017-008}.
}
\author{Francisco Benitez}


\seealso{
  \code{\link[=nsga]{nsga()}},
  \code{\link[=nsga2]{nsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}

