\name{rmooControl}
\alias{rmooControl}

\title{A function for setting or retrieving defaults non-dominated genetic operators}

\description{Default settings for non-dominated genetic operators used in the 'rmoo' package.}

\usage{
  rmooControl(\dots)
}

\arguments{
  \item{\dots}{
    no arguments, a single character vector, or a named list with components.
  }}

\details{
  If the function is called with no arguments returns the current default settings, i.e., a list with the following default components:
    \itemize{
      \item{\code{"binary"}}{
        \itemize{
          \item{\code{population}} = \code{"rmoobin_Population"}
          \item{\code{selection}} = \code{"rmoobin_tourSelection"}
          \item{\code{crossover}} = \code{"rmoobin_spCrossover"}
          \item{\code{mutation}} = \code{"rmoobin_raMutation"}
        }
      }
      \item{\code{"real-valued"}}{
        \itemize{
          \item{\code{population}} = \code{"rmooreal_Population"}
          \item{\code{selection}} = \code{"rmooreal_tourSelection"}
          \item{\code{crossover}} = \code{"rmooreal_sbxCrossover"}
          \item{\code{mutation}} = \code{"rmooreal_polMutation"}
        }
      }
      \item{\code{"permutation"}}{
        \itemize{
          \item{\code{population}} = \code{"rmooperm_Population"}
          \item{\code{selection}} = \code{"rmooperm_tourSelection"}
          \item{\code{crossover}} = \code{"rmooperm_oxCrossover"}
          \item{\code{mutation}} = \code{"rmooperm_simMutation"}
        }
      }
      \item{\code{"discrete"}}{
        \itemize{
          \item{\code{population}} = \code{"rmooint_Population"}
          \item{\code{selection}} = \code{"rmooint_tourSelection"}
          \item{\code{crossover}} = \code{"rmooint_uxCrossover"}
          \item{\code{mutation}} = \code{"rmooint_uxMutation"}
        }
      }

      \item{\code{"eps"} = }{the tolerance value used by the package functions. By default set at \code{sqrt(.Machine$double.eps)}.}
    }

  The function may be called with a single string specifying the name of the component. In this case the function returns the current default settings.

  To change the default values, a named component must be followed by a single value (in case of \code{"eps"}) or a list of component(s) specifying the name of the function for a genetic operator. See the Examples section.
}

\value{
  If the argument list is empty the function returns the current list of values.
  If the argument list is not empty, the returned list is invisible.
}

\note{
  The parameter values set via a call to this function will remain in effect for the rest of the session, affecting the subsequent behaviour of the functions for which the given parameters are relevant.
}

\author{Francisco Benitez}

\references{
  Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206, doi: 10.32614/RJ-2017-008.
}

\seealso{
  \code{\link[=nsga2]{nsga2()}},
  \code{\link[=rnsga2]{rnsga2()}} and
  \code{\link[=nsga3]{nsga3()}}
}

\examples{
  # get and save defaults
  defaultControl <- rmooControl()
  print(defaultControl)
  # get current defaults only for real-valued search
  rmooControl("real-valued")
  # set defaults for selection operator of real-valued search
  rmooControl("real-valued" = list(selection = "rmooreal_lrSelection"))
  rmooControl("real-valued")
  # set defaults for selection and crossover operators of real-valued search
  rmooControl("real-valued" = list(selection = "rmooreal_lrSelection",
                                   crossover = "rmooreal_spCrossover"))
  rmooControl("real-valued")
  # restore defaults
  rmooControl(defaultControl)
  rmooControl()
}
