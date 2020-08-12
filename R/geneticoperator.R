#Population
##
## Real Value NSGA operators  ----
##

# Generate a real random population ----
#1
nsgareal_Population <- function(object) {
  lower <- object@lower
  upper <- object@upper
  nvars <- length(lower)
  population <- matrix(as.double(NA), nrow = object@popSize, ncol = nvars)
  for(j in 1:nvars){
    population[,j] <- runif(object@popSize, lower[j], upper[j])
  }
  return(population)
}

##
## Binary NSGA operators  ----
##

# Generate a binary random population ----
nsgabin_Population <- function(object)
{
  population <- matrix(as.double(NA),
    nrow = object@popSize,
    ncol = object@nBits)
  for(j in 1:object@nBits)
  { population[,j] <- round(runif(object@popSize)) }
  storage.mode(population) <- "integer"
  return(population)
}

##
## Permutation NSGA operators  ----
##

nsgaperm_Population <- function(object)
{
  int <- seq.int(object@lower, object@upper)
  n <- length(int)
  population <- matrix(NA, nrow = object@popSize, ncol = n)
  for(i in 1:object@popSize)
    population[i,] <- sample(int, replace = FALSE)
  return(population)
}
#2
# function(lower, upper, popSize){
#   nvars <- length(lower)
#   population <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
#   for(j in 1:nvars){
#     population[,j] <- runif(popSize, lower[j], upper[j])
#   }
#   return(population)
# }

#Selection
#1 For NSGA-II
nsga_tourSelection <- function(object, k = 3, ...) {
  popSize <- object@popSize
  front <- object@front
  cd <- object@crowdingDistance
  sel <- rep(NA, popSize)
  for (i in 1:popSize) {
    s <- sample(1:popSize, size = k)
    s <- s[which(front[s,]==min(front[s, ]))]
    if(length(s)>1 & !anyNA(cd[s,])){
      sel[i] <- s[which(cd[s, ] == max(cd[s]))]
    } else {
      sel[i] <- s[which.min(front[s, ])]
    }
  }
  out <- list(population = object@population[sel, ],
    fitness = object@fitness[sel,])
  return(out)
}

nsga_lrSelection_R <- function(object, nObj, r, q)
{
  if(missing(r)) r <- 2/(object@popSize * (object@popSize - 1))
  if(missing(q)) q <- 2/object@popSize
  rank <- (object@popSize+1) - rank(object@fitness, ties.method = "min")
  prob <- 1 + q - (rank-1)*r
  prob <- pmin(pmax(0, prob/sum(prob)), 1, na.rm = TRUE)
  sel <- sample(1:object@popSize, size = object@popSize,
    prob = prob, replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
    fitness = object@fitness[sel])
  return(out)
}

nsgabin_lrSelection <- nsga_lrSelection_R
nsgaperm_lrSelection <- nsga_lrSelection_R
#2 For all
# nsgareal_tourSelection <- function(object, k = 3, ...) {
#   popSize <- object@popSize
#   front <- object@front
#   fit <- object@fitness
#   sel <- rep(NA, popSize)
#   for (i in 1:popSize) {
#     s <- sample(1:popSize, size = k)
#     s <- s[which(front[s,]==min(front[s, ]))]
#     if(length(s)>1 & !anyNA(fit[s,])){
#       sel[i] <- s[which(fit[s, ] == max(fit[s]))]
#     } else {
#       sel[i] <- s[which.min(front[s, ])]
#     }
#   }
#   out <- list(population = object@population[sel, ],
#     fitness = object@fitness[sel,])
#   return(out)
# }
nsgareal_tourSelection <- nsga_tourSelection
nsgabin_tourSelection <- nsga_tourSelection
nsgaperm_tourSelection <- nsga_tourSelection
#2
# function(popSize, front, cd, fitness, population, k = 3, ...) {
#   sel <- rep(NA, popSize)
#   for (i in 1:popSize) {
#     s <- sample(1:popSize, size = k)
#     s <- s[which(front[s, ] == min(front[s, ]))]
#     if (length(s)>1 & !anyNA(cd[s, ])) {
#       sel[i] <- s[which(cd[s, ]==max(cd[s]))]
#     } else {
#       sel[i] <- s[which.min(front[s, ])]
#     }
#   }
#   out <- list(pop = population[sel, ],
#               fit = fitness[sel, ])
#   return(out)
# }


#Crossover
#1
nsgareal_sbxCrossover <- function(object, parents, nc = 20) {
  parents <- object@population[parents, ]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  for (j in 1:n) {
    parent1 <- parents[1, j]
    parent2 <- parents[2, j]
    yl <- object@lower[j]
    yu <- object@upper[j]
    rnd <- runif(1)
    if (rnd <= 0.5) {
      if (abs(parent1 - parent2) > 1e-06) {
        if (parent2 > parent1) {
          y2 <- parent2
          y1 <- parent1
        } else {
          y2 <- parent1
          y1 <- parent2
        }
        if ((y1 - yl) > (yu - y2)) {
          beta <- 1 + (2 * (yu - y2) / (y2 - y1))
        } else {
          beta <- 1 + (2 * (y1 - yl) / (y2 - y1))
        }
        alpha <- 2 - (beta^(-(1 + nc)))
        rnd <- runif(1)
        if (rnd <= 1 / alpha) {
          alpha <- alpha * rnd
          betaq <- alpha ^ (1 / (1 + nc))
        } else {
          alpha <- alpha * rnd
          alpha <- 1 / (2 - alpha)
          betaq <- alpha ^ (1 / (1 + nc))
        }
        child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
        child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
      } else {
        betaq <- 1
        y1 <- parent1
        y2 <- parent2
        child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
        child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
      }
      if (child1 > yu) {
        child1 <- yu
      } else if (child1 < yl) {
        child1 <- yl
      }
      if (child2 > yu) {
        child2 <- yu
      } else if (child2 < yl) {
        child2 <- yl
      }
    } else {
      child1 <- parent1
      child2 <- parent2
    }
    children[1, j] <- child1
    children[2, j] <- child2
  }
  out <- list(children = children,
              fitness = matrix(as.double(NA),ncol = n))
  return(out)
}
#2
# function (population, lower, upper, parents, nc = 20) {
#   parents <- population[parents, ]
#   n <- ncol(parents)
#   children <- matrix(as.double(NA), nrow = 2, ncol = n)
#   for (j in 1:n) {
#     parent1 <- parents[1, j]
#     parent2 <- parents[2, j]
#     yl <- lower[j]
#     yu <- upper[j]
#     rnd = runif(1)
#     if (rnd <= 0.5) {
#       if (abs(parent1 - parent2) > 1e-06) {
#         if (parent2 > parent1) {
#           y2 <- parent2
#           y1 <- parent1
#         } else {
#           y2 <- parent1
#           y1 <- parent2
#         }
#         if ((y1 - yl) > (yu - y2)) {
#           beta <- 1 + (2 * (yu - y2)/(y2 - y1))
#         }
#         else {
#           beta <- 1 + (2 * (y1 - yl)/(y2 - y1))
#         }
#         alpha <- 2 - (beta^(-(1 + nc)))
#         rnd <- runif(1)
#         if (rnd <= 1/alpha) {
#           alpha <- alpha * rnd
#           betaq <- alpha ^ (1 / (1 + nc))
#         } else {
#           alpha <- alpha * rnd
#           alpha <- 1 / (2 - alpha)
#           betaq <- alpha ^ (1/(1 + nc))
#         }
#         child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
#         child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
#       } else {
#         betaq  <- 1
#         y1 <- parent1
#         y2 <- parent2
#         child1 <- 0.5 * ((y1 + y2) - betaq * (y2 - y1))
#         child2 <- 0.5 * ((y1 + y2) + betaq * (y2 - y1))
#       }
#       if (child1 > yu) {
#         child1 <- yu
#       } else if (child1 < yl) {
#         child1 <- yl
#       }
#       if (child2 > yu) {
#         child2 <- yu
#       } else if (child2 < yl) {
#         child2 <- yl
#       }
#     } else {
#       child1 <- parent1
#       child2 <- parent2
#     }
#     children[1, j] <- child1
#     children[2, j] <- child2
#   }
#   out <- list(children = children,
#               fit = matrix(as.double(NA),ncol = n))
#   return(out)
# }

nsga_spCrossover_R <- function(object, parents) {
  fitness <- object@fitness[parents, ]
  parents <- object@population[parents, ]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  crossOverPoint <- sample(0:n, size = 1)
  if (crossOverPoint == 0) {
    fitnessChildren <- matrix(as.double(NA), nrow = 2, ncol = 2)
    children[1:2,] <- parents[2:1,]
    fitnessChildren[1:2,] <- fitness[2:1,]
  } else if(crossOverPoint == n) {
    children <- parents
    fitnessChildren <- fitness
  } else {
    fitnessChildren <- rep(NA, 2)
    children[1,] <- c(parents[1,1:crossOverPoint],
                      parents[2,(crossOverPoint+1):n])
    children[2,] <- c(parents[2,1:crossOverPoint],
                      parents[1,(crossOverPoint+1):n])
  }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}

nsgabin_spCrossover <- nsga_spCrossover_R

nsgaperm_oxCrossover <- function(object, parents) {
  parents <- object@population[parents, ]
  n <- ncol(parents)
  #
  cxPoints <- sample(seq(2, n-1), size = 2)
  cxPoints <- seq(min(cxPoints), max(cxPoints))
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  children[, cxPoints] <- parents[, cxPoints]
  #
  for (j in 1:2) {
    pos <- c((max(cxPoints)+1):n, 1:(max(cxPoints)))
    val <- setdiff(parents[-j, pos], children[j, cxPoints])
    ival <- intersect(pos, which(is.na(children[j, ])))
    children[j, ival] <- val
  }
  #
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}



#Mutation
#1 OK
nsgareal_polMutation <- function(object, parent, nm = 0.20){
  mutate <- parent <- as.vector(object@population[parent, ])
  n <- length(parent)
  upper <- object@upper
  lower <- object@lower
  delta <- upper - lower
  delta1 <- (mutate - lower) / (upper - lower)
  delta2 <- (upper - mutate) / (upper - lower)
  mut_pow <- 1.0 / (nm + 1.0)
  u <- runif(1)
  if (u <= 0.5) {
    xy <- 1.0 - delta1
    val <- 2.0 * u + (1.0 - 2.0 * u) * (xy ^ (nm + 1.0))
    deltaq <- (val ^ mut_pow) - 1.0
  } else {
    xy = 1.0 - delta2
    val = 2.0 * (1.0 - u) + 2.0 * (u - 0.5) * (xy ^ (nm + 1.0))
    deltaq = 1.0 - (val ^ mut_pow)
  }
  mutate <- deltaq * delta
  for (i in 1:n) {
    if (mutate[i] < lower[i]){
      mutate[i] <- lower[i]
    }
    if (mutate[i] > upper[i]){
      mutate[i] <- upper[i]
    }
  }
  return(mutate)
}
#2
# function(population, lower, upper, parent, nm = 0.20){
#   mutate <- parent <- as.vector(population[parent, ])
#   n <- length(parent)
#   delta <- upper - lower
#   delta1 <- (mutate - lower) / (upper - lower)
#   delta2 <- (upper - mutate) / (upper - lower)
#   mut_pow <- 1.0 / (nm + 1.0)
#   u <- runif(1)
#   if (u <= 0.5) {
#     xy <- 1.0 - delta1
#     val <- 2.0 * u + (1.0 - 2.0 * u) * (xy ^ (nm + 1.0))
#     deltaq <- (val ^ mut_pow) - 1.0
#   } else {
#     xy = 1.0 - delta2
#     val = 2.0 * (1.0 - u) + 2.0 * (u - 0.5) * (xy ^ (nm + 1.0))
#     deltaq = 1.0 - (val ^ mut_pow)
#   }
#   mutate <- deltaq * delta
#   for (i in 1:n) {
#     if (mutate[i] < lower[i]){
#       mutate[i] <- lower[i]
#     }
#     if (mutate[i] > upper[i]){
#       mutate[i] <- upper[i]
#     }
#   }
#   return(mutate)
# }
#
# #2 NO
# nsgareal_polMutation_R <- function(population, lower, upper, parent, nm = 20) {
#   mutate <- parent <- as.vector(population[parent,])
#   n <- length(parent)
#   delta <- upper - lower
#   u <- runif(1)
#   j <- sample(1:n, size = 1)
#   d <- min((mutate[j] - lower[j]), (upper[j] - mutate[j]))
#   d <- d / delta
#   if (u <= 0.5) {
#     dq <- ((2 * u + (1 - 2 * u) * (((1 - d) ^ (nm + 1)) ^ (1 / (nm + 1))))) - 1
#   }else{
#     dq <- 1 - ((2 * (1 - u) + 2 * (u - 0.5) * ((1 - d) ^ (nm + 1))) ^ (1 / (nm + 1)))
#   }
#   mutate[j] <- mutate[j] + dq * delta
#   return(mutate)
#
# }


nsgareal_raMutation <- mutation <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- runif(1, object@lower[j], object@upper[j])
  return(mutate)
}

nsgabin_raMutation <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}

nsgaperm_simMutation <- function(object, parent)
{
  parent <- as.vector(object@population[parent,])
  n <- length(parent)
  m <- sort(sample(1:n, size = 2))
  m <- seq(m[1], m[2], by = 1)
  if(min(m)==1 & max(m)==n)
    i <- rev(m)
  else if(min(m)==1)
    i <- c(rev(m), seq(max(m)+1, n, by = 1))
  else if(max(m)==n)
    i <- c(seq(1, min(m)-1, by = 1), rev(m))
  else i <- c(seq(1, min(m)-1, by = 1), rev(m), seq(max(m)+1, n, by = 1))
  mutate <- parent[i]
  return(mutate)
}
