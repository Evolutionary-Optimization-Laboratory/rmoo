\name{associate}
\alias{associate}
%
\alias{associate_to_niches}
\alias{compute_perpendicular_distance}
\alias{compute_niche_count}

\title{Association Operation in Non-Dominated Genetic Algorithms III}

\description{Function that associates each member of the population with a reference point. The function calculates the perpendicular distance of each individual from each of the reference lines}

\usage{
  associate_to_niches(object, utopian_epsilon = 0)
  compute_perpendicular_distance(x, y)
  compute_niche_count(n_niches, niche_of_individuals)
}

\arguments{
  \item{object}{An object of class \code{"nsga3"}.}
  
  \item{utopian_epsilon}{The epsilon used for decrease the ideal point to get the utopian point.}
  
  \item{x}{Individuals to calculate their niche.}

  \item{y}{Reference points.}

  \item{n_niches}{Number of reference points.}

  \item{niche_of_individuals}{The niche count of individuals, except the last front.}
}

\value{Returns a list with the niche count of individuals and the distances between them.}

\references{
  J. Blank and K. Deb, "Pymoo: Multi-Objective Optimization in Python," in IEEE Access, vol. 8, pp. 89497-89509, 2020, doi: 10.1109/ACCESS.2020.2990567.

  K. Deb and H. Jain, "An Evolutionary Many-Objective Optimization Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I: Solving Problems With Box Constraints," in IEEE Transactions on Evolutionary Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014, doi: 10.1109/TEVC.2013.2281535.
}

\author{Francisco Benitez}
