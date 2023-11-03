\name{performance_metrics}
\alias{performance_metrics}
%
\alias{generational_distance}

\title{Objective Values performance metrics}

\description{Functions to evaluate the quality of the results obtained by the algorithms, evaluating their diversity and convergence, providing or not some parameters to compare.}

\usage{
  generational_distance(front, true_pareto_front, p, inverted, plus)

}

\arguments{
  \item{front}{a N×M matrix where N is the number of points and M is the number of objectives.}

  \item{true_pareto_front}{a N×M matrix where N is the number of points and M is the number of objectives.}

  \item{p}{is the power in which the normalized distance is calculated.}

  \item{inverted}{if TRUE then computes IGD.}

  \item{plus}{if TRUE then computes the GD+.}
}

\value{A vector with the measurement metric.}

\references{
  Lamont, G., & Veldhuizen, D.V. (1999). Multiobjective evolutionary algorithms: classifications, analyses, and new innovations.

}

\author{Francisco Benitez}
