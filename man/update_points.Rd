\name{update_points}
\alias{update_points}
%
\alias{UpdateIdealPoint}
\alias{UpdateWorstPoint}
\alias{PerformScalarizing}
\alias{get_nadir_point}

\title{Adaptive normalization of population members}

\description{Functions to scalarize the members of the population to locate them in a normalized hyperplane, finding the ideal point, nadir point, worst point and the extreme points.}

\usage{
  UpdateIdealPoint(object, nObj)
  UpdateWorstPoint(object, nObj)
  PerformScalarizing(population, fitness, smin, extreme_points, ideal_point)
  get_nadir_point(object)
}

\arguments{
  \item{object}{An object of class \code{"nsga3"}.}
  
  \item{nObj}{numbers of objective values of the function to evaluate.}
  
  \item{population}{individuals of the population until last front.}

  \item{fitness}{objective values of the population until last front.}

  \item{smin}{Achievement Escalation Function Index.}

  \item{extreme_points}{Extreme points of the previous generation to upgrade.}

  \item{ideal_point}{Ideal point of the current generation to translate objectives.}
}

\value{Return scalarized objective values in a normalized hyperplane.}

\references{
  J. Blank and K. Deb, "Pymoo: Multi-Objective Optimization in Python," in IEEE Access, vol. 8, pp. 89497-89509, 2020, doi: 10.1109/ACCESS.2020.2990567.

  K. Deb and H. Jain, "An Evolutionary Many-Objective Optimization Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I: Solving Problems With Box Constraints," in IEEE Transactions on Evolutionary Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014, doi: 10.1109/TEVC.2013.2281535.
}

\author{Francisco Benitez}
