# Real Value NSGA operators: Generate a real random population ----
#' @export
rmooreal_Population <- function(object) {
    lower <- object@lower
    upper <- object@upper
    nvars <- length(lower)
    population <- matrix(NA_real_, nrow = object@popSize, ncol = nvars)
    for (j in 1:nvars) {
        population[, j] <- runif(object@popSize, lower[j], upper[j])
    }
    return(population)
}

# Binary NSGA operators: Generate a binary random population ----
#' @export
rmoobin_Population <- function(object) {
    population <- matrix(NA_real_,
                         nrow = object@popSize,
                         ncol = object@nBits)
    for (j in 1:object@nBits) {
        population[, j] <- round(runif(object@popSize))
    }
    storage.mode(population) <- "integer"
    return(population)
}

# Permutation NSGA operators: Generate a permutation random population ----
#' @export
rmooperm_Population <- function(object) {
    int <- seq.int(object@lower, object@upper)
    n <- length(int)
    population <- matrix(NA, nrow = object@popSize, ncol = n)
    for (i in 1:object@popSize)
      population[i, ] <- sample(int, replace = FALSE)
    return(population)
}

# Integer NSGA operators: Generate a discrete random population ----
#' @export
rmooint_Population <- function(object) {
  lower <- object@lower
  upper <- object@upper
  popSize <- object@popSize
  nvars <- object@nvars

  population <- matrix(sample(x = lower:upper,
                              size = popSize * nvars,
                              replace = TRUE),
                       ncol = nvars,
                       nrow = popSize)

  storage.mode(population) <- "integer"
  return(population)
}
# rmooint_Population <- function(object) {
#   lower <- object@lower
#   upper <- object@upper
#   popSize <- object@popSize
#   nvars <- object@nvars
#
#   population <- matrix(sample(x = lower:upper,
#                               size = popSize * nvars,
#                               replace = TRUE),
#                        nrow = popSize,
#                        ncol = nvars)
#   storage.mode(population) <- "integer"
#   return(population)
# }
#
# rmooint_Population <- function(object) {
#   lower <- object@lower
#   upper <- object@upper
#   popSize <- object@popSize
#   nvars <- object@nvars
#
#   population <- matrix(NA_integer_, nrow = popSize, ncol = nvars)
#   for (i in 1:popSize) {
#     population[i, ] <- sample(lower:upper, nvars, replace = TRUE)
#   }
#   storage.mode(population) <- "integer"
#   return(population)
# }

## Selection Operators ---- //Change to method
#' @export
rmoo_tourSelection <- function(object, k = 2, ...) {
  class_object <- class(object)[1]
  if (class_object == "nsga2") {
    sel <- binary_tournament(object, k)
  } else if (class_object == "rnsga2" || class_object == "nsga3") {
    sel <- comp_by_cv_then_random(object, k)
    # popSize <- object@popSize
    # front <- object@front
    # fit <- object@fitness
    # sel <- rep(NA, popSize)
    # for (i in seq_len(popSize)) {
    #   s <- sample(seq_len(popSize), size = k)
    #   s <- s[which.min(front[s, ])]
    #   if (length(s) > 1 & !anyNA(fit[s, ])) {
    #     sel[i] <- s[which.max(front[s, ])]
    #   } else {
    #       sel[i] <- s[which.min(front[s, ])]
    #   }
    # }
  }
  out <- list(population = object@population[sel, ],
              fitness = object@fitness[sel, ])
  return(out)
}


# rmoo_tourSelection <- function(object, k = 2, ...) {
#   class_object <- class(object)[1]
#
#   if (class_object == "nsga2") {
#     popSize <- object@popSize
#     front <- object@front
#     cd <- crowding_distance(object, ncol(object@fitness))
#     sel <- rep(NA, popSize)
#     for (i in seq_len(popSize)) {
#       s <- sample(seq_len(popSize), size = k)
#       s <- s[which.min(front[s, ])]
#       if (!anyNA(cd[s, ])) {
#         sel[i] <- s[which.max(cd[s, ])]
#       } else {
#         sel[i] <- s[which.min(front[s, ])]
#       }
#     }
  # } else if (class_object == "rnsga2" || class_object == "nsga3") {
  #   popSize <- object@popSize
  #   front <- object@front
  #   fit <- object@fitness
  #   sel <- rep(NA, popSize)
  #   for (i in seq_len(popSize)) {
  #     s <- sample(seq_len(popSize), size = k)
  #     s <- s[which.min(front[s, ])]
  #     if (length(s) > 1 & !anyNA(fit[s, ])) {
  #       sel[i] <- s[which.max(front[s, ])]
  #     } else {
  #       sel[i] <- s[which.min(front[s, ])]
  #     }
  #   }
  # }
#   out <- list(population = object@population[sel, ],
#               fitness = object@fitness[sel, ])
#   return(out)
# }

# rmoo_tourSelection <- function(object, k = 3, ...) {
#     switch(class(object)[1], nsga1 = {
#         popSize <- object@popSize
#         front <- object@front
#         fit <- object@dumFitness
#         sel <- rep(NA, popSize)
#         for (i in 1:popSize) {
#             s <- sample(1:popSize, size = k)
#             s <- s[which.min(front[s, ])]
#             if (length(s) > 1 & !anyNA(fit[s, ])) {
#                 sel[i] <- s[which.max(front[s, ])]
#             } else {
#                 sel[i] <- s[which.min(front[s, ])]
#             }
#         }
#         out <- list(population = object@population[sel, ],
#                     fitness = object@fitness[sel, ])
#         return(out)
#     }, nsga2 = {
#         popSize <- object@popSize
#         front <- object@front
#         cd <- object@crowdingDistance
#         sel <- rep(NA, popSize)
#         for (i in 1:popSize) {
#             s <- sample(1:popSize, size = k)
#             s <- s[which.min(front[s, ])]
#             if (!anyNA(cd[s, ])) {
#                 sel[i] <- s[which.max(cd[s, ])]
#             } else {
#                 sel[i] <- s[which.min(front[s, ])]
#             }
#         }
#         out <- list(population = object@population[sel, ],
#                     fitness = object@fitness[sel, ])
#         return(out)
#     }, rnsga2 = {
#       popSize <- object@popSize
#       front <- object@front
#       fit <- object@fitness
#       sel <- rep(NA, popSize)
#       for (i in 1:popSize) {
#         s <- sample(1:popSize, size = k)
#         s <- s[which.min(front[s, ])]
#         if (length(s) > 1 & !anyNA(fit[s, ])) {
#           sel[i] <- s[which.max(front[s, ])]
#         } else {
#           sel[i] <- s[which.min(front[s, ])]
#         }
#       }
#       out <- list(population = object@population[sel, ],
#                   fitness = object@fitness[sel, ])
#       return(out)
#     }, nsga3 = {
#         popSize <- object@popSize
#         front <- object@front
#         fit <- object@fitness
#         sel <- rep(NA, popSize)
#         for (i in 1:popSize) {
#             s <- sample(1:popSize, size = k)
#             s <- s[which.min(front[s, ])]
#             if (length(s) > 1 & !anyNA(fit[s, ])) {
#                 sel[i] <- s[which.max(front[s, ])]
#             } else {
#                 sel[i] <- s[which.min(front[s, ])]
#             }
#         }
#         out <- list(population = object@population[sel, ],
#                     fitness = object@fitness[sel, ])
#         return(out)
#     })
# }

#' @export
rmooreal_tourSelection <- rmoo_tourSelection
#' @export
rmoobin_tourSelection <- rmoo_tourSelection
#' @export
rmooperm_tourSelection <- rmoo_tourSelection
# @export
rmooint_tourSelection <- rmoo_tourSelection

#' @export
rmoo_lrSelection <- function(object, r, q) {
  if (missing(r))
    r <- 2 / (object@popSize * (object@popSize - 1))
  if (missing(q))
    q <- 2 / object@popSize
  rank <- (object@popSize + 1) - as.vector(object@front)
  prob <- 1 + q - (rank - 1) * r
  prob <- pmin(pmax(0, prob / sum(prob)), 1, na.rm = TRUE)
  sel <- sample(1:object@popSize,
                size = object@popSize,
                prob = prob, replace = TRUE)
  out <- list(population = object@population[sel, ],
              fitness = object@fitness[sel, ])
  return(out)
}

#' @export
rmoobin_lrSelection <- rmoo_lrSelection

#' @export
rmooperm_lrSelection <- rmoo_lrSelection

#' @export
rmooreal_lrSelection <- rmoo_lrSelection

# @export
rmooint_lrSelection <- rmoo_lrSelection

## Crossover Operators ----
#' @export
rmooreal_sbxCrossover <- function(object, parents, eta = 20, indpb = 0.5) {
  parents <- object@population[parents, ]
  n <- ncol(parents)
  nObj <- ncol(object@fitness)
  children <- matrix(NA_real_, nrow = 2, ncol = n)

  parent1 <- parents[1, ]
  parent2 <- parents[2, ]

  yl <- object@lower
  yu <- object@upper

  for (i in 1:n) {
    if (runif(1) <= indpb) {
      if (abs(parent1[i] - parent2[i]) > 1e-4) {
        x1 <- min(parent1[i], parent2[i])
        x2 <- max(parent1[i], parent2[i])
        rand <- runif(1)

        beta <- 1 + 2 * (x1 - yl[i]) / (x2 - x1)
        alpha <- 2 - beta^(-(eta + 1))

        if (rand <= (1 / alpha)) {
          beta_q <- (rand * alpha)^(1 / (eta + 1))
        } else {
          beta_q <- (1 / (2 - rand * alpha))^(1 / (eta + 1))
        }

        c1 <- 0.5 * (x1 + x2 - beta_q * (x2 - x1))

        beta <- 1 + 2 * (yu[i] - x2) / (x2 - x1)
        alpha <- 2 - beta^-(eta + 1)

        if (rand <= (1 / alpha)) {
          beta_q <- (rand * alpha)^(1 / (eta + 1))
        } else {
          beta_q <- (1 / (2 - rand * alpha))^(1 / (eta + 1))
        }

        c2 <- 0.5 * (x1 + x2 + beta_q * (x2 - x1))

        c1 <- pmin(pmax(c1, yl[i]), yu[i])
        c2 <- pmin(pmax(c2, yl[i]), yu[i])

        if (runif(1) <= 0.5) {
          parent1[i] <- c2
          parent2[i] <- c1
        } else {
          parent1[i] <- c1
          parent2[i] <- c2
        }
      }
    }
  }
  children[1, ] <- parent1
  children[2, ] <- parent2

  out <- list(children = children,
              fitness = matrix(NA_real_, ncol = nObj))

  return(out)
}

# rmooreal_sbxCrossover <- function(object, parents, nc = 20) {
#     parents <- object@population[parents, ]
#     n <- ncol(parents)
#     nObj <- ncol(object@fitness)
#     children <- matrix(NA_real_, nrow = 2, ncol = n)
#     for (j in 1:n) {
#         parent1 <- parents[1, j]
#         parent2 <- parents[2, j]
#         yl <- object@lower[j]
#         yu <- object@upper[j]
#         rnd <- runif(1)
#         if (rnd <= 0.5) {
#             if (abs(parent1 - parent2) > 1e-06) {
#                 if (parent2 > parent1) {
#                   y2 <- parent2
#                   y1 <- parent1
#                 } else {
#                   y2 <- parent1
#                   y1 <- parent2
#                 }
#                 if ((y1 - yl) > (yu - y2)) {
#                   beta <- 1 + (2 * (yu - y2) / (y2 - y1))
#                 } else {
#                   beta <- 1 + (2 * (y1 - yl) / (y2 - y1))
#                 }
#                 alpha <- 2 - (beta^(-(1 + nc)))
#                 rnd <- runif(1)
#                 if (rnd <= 1 / alpha) {
#                   alpha <- alpha * rnd
#                   betaq <- alpha^(1 / (1 + nc))
#                 } else {
#                   alpha <- alpha * rnd
#                   alpha <- 1 / (2 - alpha)
#                   betaq <- alpha^(1 / (1 + nc))
#                 }
#                 child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
#                 child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
#             } else {
#                 betaq <- 1
#                 y1 <- parent1
#                 y2 <- parent2
#                 child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
#                 child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
#             }
#             if (child1 > yu) {
#                 child1 <- yu
#             } else if (child1 < yl) {
#                 child1 <- yl
#             }
#             if (child2 > yu) {
#                 child2 <- yu
#             } else if (child2 < yl) {
#                 child2 <- yl
#             }
#         } else {
#             child1 <- parent1
#             child2 <- parent2
#         }
#         children[1, j] <- child1
#         children[2, j] <- child2
#     }
#     out <- list(children = children,
#                 fitness = matrix(NA_real_, ncol = nObj))
#     return(out)
# }

#' @export
rmoo_spCrossover <- function(object, parents) {
    fitness <- object@fitness[parents, ]
    parents <- object@population[parents, ]
    n <- ncol(parents)
    children <- matrix(NA_real_, nrow = 2, ncol = n)
    crossOverPoint <- sample(0:n, size = 1)
    if (crossOverPoint == 0) {
        fitnessChildren <- matrix(NA_real_, nrow = 2, ncol = ncol(fitness))
        children[1:2, ] <- parents[2:1, ]
        fitnessChildren[1:2, ] <- fitness[2:1, ]
    } else if (crossOverPoint == n) {
        children <- parents
        fitnessChildren <- fitness
    } else {
        fitnessChildren <- rep(NA, 2)
        children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, (crossOverPoint + 1):n])
        children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, (crossOverPoint + 1):n])
    }
    out <- list(children = children,
                fitness = fitnessChildren)
    return(out)
}
#' @export
rmoobin_spCrossover <- rmoo_spCrossover
#' @export
rmooreal_spCrossover <- rmoo_spCrossover
#' @export
rmooint_spCrossover <- rmoo_spCrossover

#' @export
rmooperm_oxCrossover <- function(object, parents) {
    parents <- object@population[parents, ]
    n <- ncol(parents)
    #
    cxPoints <- sample(seq(2, n - 1), size = 2)
    cxPoints <- seq(min(cxPoints), max(cxPoints))
    children <- matrix(NA_real_, nrow = 2, ncol = n)
    children[, cxPoints] <- parents[, cxPoints]
    #
    for (j in 1:2) {
        pos <- c((max(cxPoints) + 1):n, 1:(max(cxPoints)))
        val <- setdiff(parents[-j, pos], children[j, cxPoints])
        ival <- intersect(pos, which(is.na(children[j, ])))
        children[j, ival] <- val
    }
    #
    out <- list(children = children, fitness = rep(NA, 2))
    return(out)
}

#' @export
rmoo_uxCrossover <- function(object, parents) {
  parents <- object@population[parents, ]

  n_matings <- nrow(parents)
  n <- ncol(parents)

  M <- matrix(runif(n_matings * n) < 0.5, nrow = n_matings)
  fitnessChildren <- matrix(NA_integer_, ncol = ncol(object@fitness))

  children <- crossover_mask(parents, M)

  storage.mode(children) <- "integer"

  out <- list(children = children,
              fitness = fitnessChildren)
  return(out)
}
#' @export
rmooint_uxCrossover <- rmoo_uxCrossover
#' @export
rmoobin_uxCrossover <- rmoo_uxCrossover

#' @export
rmoo_huxCrossover <- function(object, parents, prob_hux=0.5) {
  parents <- object@population[parents, ]

  n_matings <- nrow(parents)
  n <- ncol(parents)

  M <- matrix(FALSE, nrow = n_matings, ncol = n)
  fitnessChildren <- matrix(NA_integer_, ncol = ncol(object@fitness))

  not_equal <- (parents[1, ] != parents[2, ])

  for (i in 1:n_matings) {
    ind <- which(not_equal[i])

    n <- ceiling(length(ind) / 2)
    if (n > 0) {
      indx <- ind[sample(length(ind), size = n)]
      M[i, indx] <- TRUE
    }
  }

  children <- crossover_mask(parents, M)
  storage.mode(children) <- "integer"

  out <- list(children = children,
              fitness = fitnessChildren)
  return(out)
}
#' @export
rmooint_huxCrossover <- rmoo_huxCrossover
#' @export
rmoobin_huxCrossover <- rmoo_huxCrossover

## Mutation Operator ----
#' @export
rmooreal_polMutation <- function(object, parent, eta=20, indpb=0.5) {
  mutate <- as.vector(object@population[parent, ])
  n <- length(parent)
  lower <- object@lower
  upper <- object@upper

  for (i in 1:n) {
    if (runif(1) <= indpb) {
      x <- mutate[i]
      delta_1 <- (x - lower[i]) / (upper[i] - lower[i])
      delta_2 <- (upper[i] - x) / (upper[i] - lower[i])
      mut_pow <- 1 / (eta + 1)

      rand <- runif(1)
      if (rand < 0.5) {
        xy <- 1 - delta_1
        val <- 2 * rand + (1 - 2 * rand) * (xy^(eta + 1))
        delta_q <- (val^mut_pow) - 1
      } else {
        xy <- 1 - delta_2
        val <- 2 * (1 - rand) + 2 * (rand - 0.5) * (xy^(eta + 1))
        delta_q <- 1 - (val^mut_pow)
      }

      x <- x + delta_q * (upper[i] - lower[i])
      x <- min(max(x, lower[i]), upper[i])
      mutate[i] <- x
    }
  }

  return(mutate)
}
# rmooreal_polMutation <- function(object, parent, nm = 0.2, indpb = 0.2) {
#   mutate <- parent <- as.vector(object@population[parent, ])
#   n <- length(parent)
#   upper <- object@upper
#   lower <- object@lower
#   delta <- upper - lower
#   delta1 <- (mutate - lower) / (upper - lower)
#   delta2 <- (upper - mutate) / (upper - lower)
#   mut_pow <- 1/(nm + 1)
#   for (i in seq_len(n)) {
#     if(runif(1) <= indpb) {
#       x <- parent[i]
#       u <- runif(1)
#       if (u <= 0.5) {
#         xy <- 1 - delta1[i]
#         val <- 2 * u + (1 - 2 * u) * (xy^(nm + 1))
#         deltaq <- (val^mut_pow) - 1
#       } else {
#         xy <- 1 - delta2[i]
#         val <- 2 * (1 - u) + 2 * (u - 0.5) * (xy^(nm + 1))
#         deltaq <- 1 - (val^mut_pow)
#       }
#       mutate[i] <- deltaq * delta[i]
#       mutate[i] <- min(max(c(x[1], lower[i])), upper[i])
#     }
#   }
#   return(mutate)
# }

#' @export
rmooreal_raMutation <- function(object, parent) {
    mutate <- parent <- as.vector(object@population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- runif(1, object@lower[j], object@upper[j])
    return(mutate)
}

#' @export
rmoobin_raMutation <- function(object, parent) {
    mutate <- parent <- as.vector(object@population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- abs(mutate[j] - 1)
    return(mutate)
}

#' @export
rmooperm_simMutation <- function(object, parent) {
    parent <- as.vector(object@population[parent, ])
    n <- length(parent)
    m <- sort(sample(1:n, size = 2))
    m <- seq(m[1], m[2], by = 1)
    if (min(m) == 1 & max(m) == n)
        i <- rev(m) else if (min(m) == 1)
        i <- c(rev(m), seq(max(m) + 1, n, by = 1))
    else if (max(m) == n)
        i <- c(seq(1, min(m) - 1, by = 1), rev(m))
    else i <- c(seq(1, min(m) - 1, by = 1), rev(m), seq(max(m) + 1, n, by = 1))
    mutate <- parent[i]
    return(mutate)
}

#' @export
rmoo_uxMutation <- function(object, parent, indpb=0.1) {
  mutate <- parent <- as.vector(object@population[parent, ])

  n <- length(parent)
  lower <- rep(object@lower,n)
  upper <- rep(object@upper,n)

  for (i in seq_along(parent)) {
    if (runif(1) < indpb) {
      mutate[i] <- sample(lower[i]:upper[i], 1)
    }
  }

  storage.mode(mutate) <- "integer"
  return(mutate)
}
#' @export
rmooint_uxMutation <- rmoo_uxMutation
#' @export
rmoobin_uxMutation <- rmoo_uxMutation

crossover_mask <- function(X, M) {
  parent <- X
  parent[1,][M[2,]] <- X[2,][M[2,]]
  parent[2,][M[1,]] <- X[1,][M[1,]]
  return(parent)
}

rmoobin_fbMutation <- function(object, parent, indpb=0.2) {
  mutate <- parent <- as.vector(object@population[parent, ])

  mutate <- as.logical(mutate)
  for (i in seq_along(parent)) {
    if (runif(1) < indpb) {
      mutate[i] <- !mutate[i]
    }
  }
  mutate <- as.numeric(mutate)
  storage.mode(mutate) <- "integer"
  return(mutate)
}

# Two Point Cx
rmoo_tpCrossover <- function(object, parents) {
  parents <- object@population[parents, ]
  n <- ncol(parents)
  children <- matrix(NA_integer_, nrow = 2, ncol = n)
  fitnessChildren <- matrix(NA_real_, ncol = ncol(object@fitness))

  ind1 <- parents[1, ]
  ind2 <- parents[2, ]

  size <- min(sum(ind1), sum(ind2))
  cxpoint1 <- sample(1:size, 1)
  cxpoint2 <- sample(1:(size - 1), 1)

  if (cxpoint2 >= cxpoint1) {
    cxpoint2 <- cxpoint2 + 1
  } else {
    temp <- cxpoint1
    cxpoint1 <- cxpoint2
    cxpoint2 <- temp
  }

  temp <- ind1[cxpoint1:cxpoint2]
  ind1[cxpoint1:cxpoint2] <- ind2[cxpoint1:cxpoint2]
  ind2[cxpoint1:cxpoint2] <- temp

  children[1,] <- ind1
  children[2,] <- ind2

  storage.mode(children) <- "integer"
  out <- list(children = children,
              fitness = fitnessChildren)
  return(out)
}

pointCrossover <- function(object, parents, n_points=2) {
  parents <- object@population[parents, ]
  n_matings <- nrow(parents)
  n <- ncol(parents)

  fitnessChildren <- matrix(NA_real_, ncol = ncol(object@fitness))

  r <- t(replicate(n_matings, sample(n - 1)))
  r <- r[, 1:n_points]
  r <- apply(r, 1, sort)
  r <- cbind(r, rep(n, n_matings))

  M <- matrix(FALSE, nrow = n_matings, ncol = n)

  for (i in seq_len(n_matings)) {
    j <- 1
    while (j < (ncol(r) - 1)) {
      a <- r[i, j]
      b <- r[i, j + 1]
      M[i, a:b] <- TRUE
      j <- j + 1
    }
  }
  # for (i in seq_len(n_matings)) {
  #   j <- seq(1, (ncol(r) - 1), by=2)
  #   a <- r[i, j]
  #   b <- r[i, j + 1]
  #   M[i, a:b] <- TRUE
  # }
  children <- crossover_mask(parents, M)

  storage.mode(children) <- "integer"
  out <- list(children = children,
              fitness = fitnessChildren)
  return(out)
}


binary_tournament <- function(object, k=2, tournament_type = "comp_by_rank_and_crowding"){
  sel <- rep(NA, object@popSize)
  s <- random_permutations(k, object@popSize)
  s <- matrix(s, nrow = object@popSize, ncol = k)

  for(i in seq_len(object@popSize)) {
    a <- s[i, 1]
    b <- s[i, 2]

    a_f <- object@fitness[a, ]
    rank_a <- object@front[a, ]
    cd_a <- object@crowdingDistance[a]

    b_f <- object@fitness[b, ]
    rank_b <- object@front[b, ]
    cd_b <- object@crowdingDistance[b]

    if (tournament_type == 'comp_by_dom_and_crowding') {
      rel <- get_relation(a=a_f, b=b_f)
      if (rel == 1) {
        sel[i] <- a
      } else if (rel == -1) {
        sel[i] <- b
      }
    } else if (tournament_type == 'comp_by_rank_and_crowding') {
      sel[i] <- compare(a, rank_a, b, rank_b, method = 'smaller_is_better')
    } else {
      stop("Unknown tournament type.")
    }

    if (is.na(sel[i])) {
      sel[i] <- compare(a, cd_a, b, cd_b, method = 'larger_is_better', return_random_if_equal = TRUE)
    }

  }
  # out <- list(population = object@population[sel, ],
  #             fitness = object@fitness[sel, ])
  return(sel)
}


comp_by_cv_then_random <- function(object, k=2) {
  sel <- rep(NA, object@popSize)
  s <- random_permutations(k, object@popSize)
  s <- matrix(s, nrow = object@popSize, ncol = k)

  for(i in seq_len(object@popSize)) {
    a <- s[i, 1]
    b <- s[i, 2]

    a_f <- object@fitness[a, ]
    rank_a <- object@front[a, ]
    # cv_a <- object@cv[a]

    b_f <- object@fitness[b, ]
    rank_b <- object@front[b, ]
    # cv_b <- object@cv[b]

    # if (cv_a > 0.0 || cv_b > 0.0) {
    #   sel[i] <- compare(a, cv_a, b, cv_b, method = 'smaller_is_better', return_random_if_equal = TRUE)
    # } else {
    sel[i] <- compare(a, rank_a, b, rank_b, method = 'smaller_is_better')
    # sel[i] <- sample(c(a, b), 1)
    # }
  }
  return(sel)
}



# roundingRepair <- function(pop) {
#   pop[] <- as.integer(round(pop))
#   return(pop)
# }

