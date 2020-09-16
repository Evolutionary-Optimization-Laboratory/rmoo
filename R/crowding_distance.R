#' Calculation of Crowding Distance
#'
#' A Crowded-comparison approach.
#'
#' The crowded-comparison operator guides the selection process at the various
#' stages of the algorithm toward a uniformly spread-out Pareto-optimal front
#'
#' @param object,nObj An object of class 'nsga2', usually resulting from a call to function nsga2.
#' Fitness Function Objective Numbers
#'
#' @author Francisco Benitez
#'
#' @references K. Deb, A. Pratap, S. Agarwal and T. Meyarivan, 'A fast and elitist multiobjective
#' genetic algorithm: NSGA-II,' in IEEE Transactions on Evolutionary Computation,
#' vol. 6, no. 2, pp. 182-197, April 2002, doi: 10.1109/4235.996017.
#'
#' @seealso [non_dominated_fronts()]
#'
#' @return A vector with the crowding-distance between individuals of a population.
crowding_distance <- function(object, nObj) {
    nFront <- length(object@f)
    popSize <- nrow(object@population)
    deltaf <- apply(object@fitness, 2, max) - apply(object@fitness, 2, min)
    crowding <- matrix(NA, nrow = popSize)
    for (i in seq_len(nFront)) {
        f <- object@f[[i]]
        n <- length(f)
        costs <- object@fitness[f, ]
        d <- matrix(0, nrow = n, ncol = nObj)
        for (j in seq_len(nObj)) {
            if (n > 1) {
                ord <- order(costs[, 1])
                srt <- costs[ord, ]
                d[ord[1], j] <- Inf
                if (n > 2) {
                  for (k in 2:(n - 1)) {
                    d[ord[k], j] <- abs(srt[(k + 1), j] - srt[(k - 1), j]) / abs(deltaf[j])
                  }
                }
                d[ord[n], j] <- Inf
            } else {
                costs <- matrix(costs, 1)
                ord <- order(costs[, 1])
                srt <- costs[ord, ]
                d[ord[1], j] <- Inf
            }
        }
        for (i in seq_len(n)) {
            crowding[f[i]] <- sum(d[i, ])
        }
    }
    return(crowding)
}

# crowdingdistance <- function(front, population, fitness, nObj) {
#   nFront <- length(front)
#   popSize <-nrow(population)
#   deltaF <- apply(fitness, 2, max) - apply(fitness, 2, min)
#   crowding <- matrix(NA, nrow = popSize)
#   for (i in seq_len(nFront)) {
#     f <- front[[i]]
#     n <- length(f)
#     costs <- fitness[f, ]
#     d <- matrix(0, nrow = n, ncol = nObj)
#     for (j in seq_len(nObj)) {
#       if (n > 1) {
#         ord <- order(costs[, 1])
#         srt <- costs[ord, ]
#         d[ord[1], j] <- Inf
#         if(n > 2) {
#           for (k in 2:(n - 1)) {
#             d[ord[k], j] <- abs(srt[(k + 1), j] - srt[(k - 1), j]) / abs(deltaF[j])
#           }
#         }
#         d[ord[n], j] <- Inf
#       } else {
#         costs <- matrix(costs, 1) ord <- order(costs[, 1])
#         srt <- costs[ord, ]
#         d[ord[1], j] <- Inf
#       }
#     }
#     for (i in seq_len(n)) {
#       crowding[f[i]] <- sum(d[i, ])
#     }
#   }
#   return(crowding)
# }
