#' Calculation of Modified Crowding Distance
#'
#' A Crowded-comparison approach.
#'
#' The crowded-comparison operator guides the selection process at the various
#' stages of the algorithm toward a uniformly spread-out Pareto-optimal front
#'
#' @param object An object of class 'rnsga2', usually resulting from a call
#' to function r-nsga2. Fitness Function Objective Numbers
#' @param epsion
#' @param weights
#' @param normalization
#' @param extreme_points_as_ref_dirs
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references Kalyanmoy Deb and J. Sundar. 2006. Reference point based
#' multi-objective optimization using evolutionary algorithms. In Proceedings of
#' the 8th annual conference on Genetic and evolutionary computation (GECCO '06).
#' Association for Computing Machinery, New York, NY, USA, 635–642.
#' doi: 10.1145/1143997.1144112
#'
#' @seealso [rnsga2()]
#'
#' @return A vector with the crowding-distance between individuals of a population.
#' @export
modifiedCrowdingDistance <- function(object,
                                     epsilon,
                                     weights = NULL,
                                     normalization = "front",
                                     extreme_points_as_ref_dirs = FALSE) {
  fitness <- object@fitness
  population <- object@population
  nObj <- ncol(object@fitness)
  fronts <- object@f
  nFront <- length(object@f)
  popSize <- object@popSize
  reference_points <- object@reference_points

  if (is.null(weights)) {
    weights <- rep((1/nObj),nObj)
  }

  ideal_point <- rep(Inf, nObj)
  nadir_point <- rep(-Inf, nObj)

  #Normalization
  if (normalization == "ever") {
    ideal_point <- apply(rbind(ideal_point,fitness), 2, min)
    nadir_point <- apply(rbind(ideal_point,fitness), 2, max)
  } else if (normalization == "front") {
    if (length(fronts[[1]]) > 1) {
      ideal_point <- apply(fitness[fronts[[1]],], 2, min)
      nadir_point <- apply(fitness[fronts[[1]],], 2, max)
    }
  } else if (normalization == "no"){
    ideal_point <- rep(1,nObj)
    nadir_point <- rep(0,nObj)

  }

  if (extreme_points_as_ref_dirs){
    ps <- PerformScalarizing(population = population[unlist(fronts), ],
                             fitness = fitness[unlist(fronts), ],
                             smin = object@smin,
                             extreme_points = reference_points,
                             ideal_point = ideal_point)
    reference_points <- rbind(reference_points, ps$extremepoint)
    smin <-  ps$indexmin
  } else{
    smin <- NULL
  }

  n_remaining <- popSize
  survivors <- c()

  distance_to_ref_points <- calc_norm_pref_distance(fitness=fitness,
                                                    ref_points=reference_points,
                                                    weight=weights,
                                                    ideal_point=ideal_point,
                                                    nadir_point=nadir_point)

  for (i in seq_len(nFront)) {
    #cat(i, " Iter: ", object@iter, "\n")
    n_remaining <- popSize - length(survivors)
    if(n_remaining==0) break
    if(length(fronts[[i]]) > 1){
      # rank_by_distance <- apply(apply(distance_to_ref_points[fronts[[i]],], 2, order), 2, order)
      rank_by_distance <- apply(apply(as.matrix(distance_to_ref_points[fronts[[i]],]), 2, order), 2, order) #We use as.matrix in the case when the distance ob to the reference points has one dimension
      ref_point_of_best_rank <- apply(rank_by_distance, 1, which.min)
    }else{
      rank_by_distance <-  order(order(distance_to_ref_points[fronts[[i]],]))
      rank_by_distance <- t(rank_by_distance)
      ref_point_of_best_rank <- which.min(rank_by_distance)
    }
    ranking <- diag(rank_by_distance[seq_len(length(fronts[[i]])), ref_point_of_best_rank])

    if (length(fronts[[i]]) < n_remaining){
      crowding <- ranking
      I <- seq_len(length(fronts[[i]]))
    } else{
      dist_to_others <- calc_norm_pref_distance(fitness=fitness[fronts[[i]],],
                                                ref_points=fitness[fronts[[i]],],
                                                weight=weights,
                                                ideal_point=ideal_point,
                                                nadir_point=nadir_point)
      diag(dist_to_others) <- Inf
      crowding <- rep(NA_real_, length(fronts[[i]]))
      not_selected <- order(ranking)

      while (length(not_selected) > 0) {
        idx <- not_selected[1]
        crowding[idx] <- ranking[idx]
        to_remove <- c(idx)

        dist <- dist_to_others[idx, not_selected]

        group <- not_selected[which(dist < epsilon)][1]

        if (!is.na(group)){
          if (length(group)){
            crowding[group] <- ranking[group] + round(length(fronts[[i]]) / 2)

            # remove group from not_selected array
            to_remove <- c(to_remove, group)


          }
        }
        not_selected <- not_selected[which(!(not_selected %in% to_remove))]

      }
      I <- order(crowding)[1:n_remaining]

    }
    survivors <- c(survivors,fronts[[i]][I])

  }
  out <- list(survivors = survivors,
              indexmin = smin,
              reference_points = reference_points)
  return(out)
}

#' @export
calc_norm_pref_distance <- function(fitness, ref_points, weight, ideal_point, nadir_point){
  if(!is.matrix(ref_points)){
    ref_points <- t(ref_points)
  }
  if(!is.matrix(fitness)){
    fitness <- t(fitness)
  }

  # Calculate the difference between fitness and ref_points
  D <- matrix(rep(fitness,
                  each=nrow(ref_points)),
              ncol = ncol(ref_points),
              byrow = FALSE) - matrix(rep(t(ref_points),nrow(fitness)),
                                      ncol = ncol(fitness),
                                      byrow = TRUE)

  # Calculate the denominator
  denom <- nadir_point - ideal_point # New
  denom[which(denom == 0)] <- 1 * 10^(-12) # New

  # Calculate the normalized preference distance
  N <- ((sweep(D, 2, denom, FUN = "/"))^2) * weight
  N <- sqrt(apply(N, 1, sum) * length(weight))

  return(matrix(N, nrow(fitness), nrow(ref_points), byrow = TRUE))

}
