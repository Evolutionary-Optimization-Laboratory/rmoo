#' Calculation of Crowding Distance
#'
#' A Crowded-comparison approach.
#'
#' The crowded-comparison operator guides the selection process at the various
#' stages of the algorithm toward a uniformly spread-out Pareto-optimal front
#'
#' @param object,nObj An object of class 'nsga2', usually resulting from a call
#' to function nsga2. Fitness Function Objective Numbers
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references K. Deb, A. Pratap, S. Agarwal and T. Meyarivan, 'A fast and
#' elitist multiobjective genetic algorithm: NSGA-II,' in IEEE Transactions on
#' Evolutionary Computation, vol. 6, no. 2, pp. 182-197, April 2002,
#' doi: 10.1109/4235.996017.
#'
#' @seealso [non_dominated_fronts()]
#'
#' @return A vector with the crowding-distance between individuals of a population.
#' @export
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
        ord <- order(costs[, j])
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


# library(Rcpp)
# library(RcppArmadillo)
# # Define the C++ function for crowding distance calculation
# cppFunction('NumericVector crowding_distance_cpp(List object, int nObj) {
#   int nFront = object.size();
#   int popSize = object["population"].nrow();
#   NumericMatrix fitness = object["fitness"];
#   NumericVector deltaf = apply(fitness, 2, max) - apply(fitness, 2, min);
#   NumericVector crowding(popSize, 0.0);
#
#   for (int front_idx = 0; front_idx < nFront; ++front_idx) {
#     IntegerVector front = object["f"][front_idx];
#     int n = front.size();
#     if (n > 1) {
#       NumericMatrix costs = fitness(front - 1, _);
#       IntegerVector ord = order(costs(_, 0));
#       NumericMatrix srt = costs(ord, _);
#       NumericMatrix d(n, nObj);
#
#       d(ord[0], _) = R_PosInf;
#       d(ord[n - 1], _) = R_PosInf;
#
#       for (int k = 1; k < n - 1; ++k) {
#         d(ord[k], _) = abs(srt(k + 1, _) - srt(k - 1, _)) / deltaf;
#       }
#       crowding[front - 1] = rowSums(d);
#     }
#   }
#   return crowding;
# }')
#
# # Define the optimized R function using Rcpp
# crowding_distance <- function(object, nObj) {
#   crowding <- crowding_distance_cpp(object, nObj)
#   return(matrix(crowding, ncol = 1))
# }

# crowding_distance <- function(object, nObj) {
#   nFront <- length(object@f)
#   popSize <- nrow(object@population)
#   deltaf <- apply(object@fitness, 2, max) - apply(object@fitness, 2, min)
#   crowding <- numeric(popSize)
#
#   for (front_idx in seq_len(nFront)) {
#     front <- object@f[[front_idx]]
#     n <- length(front)
#     if (n > 1) {
#       costs <- object@fitness[front, ]
#       ord <- order(costs[, 1])
#       srt <- costs[ord, ]
#       d <- matrix(0, nrow = n, ncol = nObj)
#
#       d[ord[1], ] <- Inf
#       d[ord[n], ] <- Inf
#
#       for (k in 2:(n - 1)) {
#         d[ord[k], ] <- abs(srt[(k + 1), ] - srt[(k - 1), ]) / abs(deltaf)
#       }
#       crowding[front] <- rowSums(d)
#     }
#   }
#   crowding <- matrix(crowding, ncol=1)
#   return(crowding)
# }

# Define the Rcpp function
# cppFunction('
# NumericVector crowding_distance_rcpp(List object_list, int nObj) {
#   int nFront = object_list.size();
#   NumericVector crowding(popSize, 0.0);
#   NumericMatrix fitness = object_list["fitness"];
#   IntegerVector population = object_list["population"];
#   NumericVector deltaf = colMax(fitness) - colMin(fitness);
#
#   for (int i = 0; i < nFront; i++) {
#     IntegerVector f = population[i];
#     int n = f.size();
#     NumericMatrix costs = fitness(f - 1, _);
#     NumericMatrix d(n, nObj);
#
#     if (n > 1) {
#       NumericVector ord = order(costs(_, 0));
#       NumericMatrix srt = costs(ord, _);
#       d(ord[0], _) = R_PosInf;
#       d(ord[n - 1], _) = R_PosInf;
#
#       if (n > 2) {
#         for (int k = 1; k < n - 1; k++) {
#           d(ord[k], _) = abs(srt(k + 1, _) - srt(k - 1, _)) / deltaf;
#         }
#       }
#     } else {
#       NumericMatrix costs_mat = matrix(costs, 1);
#       NumericVector ord = order(costs_mat(_, 0));
#       NumericMatrix srt = costs_mat(ord, _);
#       d(ord[0], _) = R_PosInf;
#     }
#
#     for (int j = 0; j < n; j++) {
#       crowding[f[j] - 1] += sum(d(j, _));
#     }
#   }
#
#   return crowding;
# }')
