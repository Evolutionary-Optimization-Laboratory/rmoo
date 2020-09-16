#' Calculation of Dummy Fitness
#'
#' Calculate of sharing distance and dummy fitness
#'
#' The sharing distance operator guides the selection process at the various
#' stages of the algorithm toward a uniformly spread-out Pareto-optimal front
#'
#' @param object An object of class 'nsga', usually resulting from a call to function nsga.
#' Fitness Function Objective Numbers
#'
#' @author Francisco Benitez
#'
#' @references N. Srinivas and K. Deb, 'Multiobjective Optimization Using Nondominated Sorting in Genetic Algorithms,'
#' in Evolutionary Computation, vol. 2, no. 3, pp. 221-248, Sept. 1994, doi: 10.1162/evco.1994.2.3.221.
#'
#' @seealso [non_dominated_fronts()]
#'
#' @return A vector with the dummy fitness.
sharing <- function(object) {
    front_count <- 1
    dShare <- object@dShare
    delta_dum <- object@deltaDummy
    nro_front <- length(object@f)
    while (front_count <= nro_front) {
        if (front_count == 1) {
            for (i in object@f[[front_count]]) {
                ind <- object@f[[front_count]]
                object@dumFitness[i, ] <- max(object@fitness)
                nichecount <- 1
                for (j in object@f[[front_count]]) {
                  if (i != j & i < j) {
                    d <- rbind(object@fitness[i, ], object@fitness[j, ])
                    distance <- dist(d, method = "euclidean")
                    if (distance <= 0.5) {
                      nichecount <- nichecount + 1
                    } else if (distance < dShare) {
                      nichecount <- nichecount + (1 - (distance/dShare)) * (1 - (distance/dShare))
                    }
                  }
                }
                object@dumFitness[i, ] <- object@dumFitness[i, ]/nichecount
            }
            minimum_dum <- min(object@dumFitness[ind, ])
        } else {
            for (i in object@f[[front_count]]) {
                ind <- object@f[[front_count]]
                object@dumFitness[i, ] <- minimum_dum - delta_dum
                for (j in object@f[[front_count]]) {
                  if (i != j) {
                    d <- rbind(object@fitness[i, ], object@fitness[j, ])
                    distance <- dist(d, method = "euclidean")
                    if (distance <= 0.5) {
                      nichecount <- nichecount + 1
                    } else if (distance < dShare) {
                      nichecount <- nichecount + (1 - (distance/dShare)) * (1 - (distance/dShare))
                    }
                  }
                }
                object@dumFitness[i, ] <- object@dumFitness[i, ]/nichecount
            }
            minimum_dum <- min(object@dumFitness[ind, ])
        }
        front_count = front_count + 1
    }
    return(object@dumFitness)
}
