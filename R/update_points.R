
##Ideal Point
#' @export
UpdateIdealPoint <- function(object, nObj) {
  cost <- object@fitness
  if (anyNA(object@ideal_point)) {
    ideal_point <- apply(cost, 2, min)
  } else {
    ideal_point <- apply(rbind(object@ideal_point, cost), 2, min)
  }
  return(ideal_point)
}

## Worst points
#' @export
UpdateWorstPoint <- function(object, nObj) {
  cost <- object@fitness
  if (anyNA(object@worst_point)) {
    worst_point <- apply(cost, 2, max)
  } else {
    worst_point <- apply(rbind(object@worst_point, cost), 2, max)
  }
  return(worst_point)
}

## Extreme points
#' @export
PerformScalarizing <- function(population, fitness, smin, extreme_points, ideal_point) {
  nPop <- nrow(population)
  nObj <- ncol(fitness)
  if (!anyNA(smin)) {
    F <- rbind(extreme_points, fitness)
  } else {
    extreme_points <- matrix(0, nObj, nObj)
    smin <- rep(Inf, nObj)
    F <- fitness
  }
  fp <- sweep(F, 2, ideal_point)
  w <- diag(1, nObj)
  w[which(w == 0)] <- 1e-06
  sminj <- apply(fp, 1, function(x) max(x/w))
  for (j in 1:nObj) {
    sminj_j <- min(sminj)
    ind <- which(sminj == sminj_j)

    if (length(ind) > 1)
      ind <- sample(ind, 1)

    if (sminj_j < smin[j]) {
      extreme_points[j, ] <- F[ind, ]
      smin[j] <- sminj_j
    }
  }
  out <- list(extremepoint = extreme_points, indexmin = smin)
  return(out)
}
# PerformScalarizing <- function(population, fitness, smin, extreme_points, ideal_point) {
#     nPop <- nrow(population)
#     nObj <- ncol(fitness)
#     if (!anyNA(smin)) {
#         F <- rbind(extreme_points, fitness)
#     } else {
#         extreme_points <- matrix(0, nObj, nObj)
#         smin <- rep(Inf, nObj)
#         F <- fitness
#     }
#     fp <- sweep(F, 2, ideal_point)
#     w <- diag(1, nObj)
#     w[which(w == 0)] <- 1e-06
#     for (j in 1:nObj) {
#         s <- rep(0, nPop)
#         for (i in 1:nPop) {
#             s[i] <- max(fp[i, ]/w[j, ])
#         }
#         sminj <- min(s)
#         ind <- which(s == sminj)
#
#         if (length(ind) > 1)
#             ind <- sample(ind, 1)
#
#         if (sminj < smin[j]) {
#             extreme_points[j, ] <- F[ind, ]
#             smin[j] <- sminj
#         }
#     }
#     out <- list(extremepoint = extreme_points, indexmin = smin)
#     return(out)
# }

## Nadir Point
#' @export
get_nadir_point <- function(object) {
  # Extract relevant data from object
  extreme_point <- object@extreme_points
  ideal_point <- object@ideal_point
  worst_point <- object@worst_point
  worst_of_front <- object@worst_of_front
  worst_of_population <- object@worst_of_population

  # Calculate nadir point
  M <- sweep(extreme_point, 2, ideal_point)
  b <- rep(1, ncol(object@fitness))

  # Check if M is a singular matrix
  if (abs(det(M)) < 1e-10) {
    nadir_point <- worst_of_front
  } else {
    plane <- solve(M, b)
    intercepts <- 1/plane
    nadir_point <- ideal_point + intercepts

    # Check for errors and handle them
    if (!all.equal(as.vector(M %*% plane), b) || any(intercepts <= 1e-05) || any(nadir_point > worst_point)) {
      nadir_point <- worst_of_front
    }
  }

  # Update nadir point with worst of population if necessary
  b <- (nadir_point - ideal_point) <= 1e-06
  nadir_point[b] <- worst_of_population[b]

  return(nadir_point)
}
# get_nadir_point <- function(object) {
#     extreme_point <- object@extreme_points
#     ideal_point <- object@ideal_point
#     worst_point <- object@worst_point
#     nObj <- ncol(object@fitness)
#     worst_of_front <- object@worst_of_front
#     worst_of_population <- object@worst_of_population
#     out <- tryCatch({
#         M <- sweep(extreme_point, 2, ideal_point)
#         b <- rep(1, nObj)
#         plane <- solve(M, b)
#         intercepts <- 1/plane
#         nadir_point <- ideal_point + intercepts
#         if (!all.equal(as.vector(M %*% plane), b) || any(intercepts <= 1e-05) || any(nadir_point > worst_point)) {
#             stop()
#         }
#         nadir_point
#     }, error = function(e) {
#         nadir_point <- worst_of_front
#         return(nadir_point)
#     })
#     b <- (out - ideal_point) <= 1e-06
#     out[b] <- worst_of_population[b]
#     return(out)
# }
