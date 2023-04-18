# Reference Point Based Non-Dominated Sorting in Genetic Algorithms II

#' @export
rnsga2 <- function(type = c("binary", "real-valued", "permutation"),
                  fitness, ...,
                  lower, upper, nBits,
                  population = nsgaControl(type)$population,
                  selection = nsgaControl(type)$selection,
                  crossover = nsgaControl(type)$crossover,
                  mutation = nsgaControl(type)$mutation,
                  reference_dirs = NULL,
                  epsilon = 0.001,
                  normalization = c("ever", "front", "no"),
                  extreme_points_as_ref_dirs = FALSE,
                  weights = NULL,
                  popSize = 50,
                  nObj = NULL,
                  pcrossover = 0.8,
                  pmutation = 0.1,
                  maxiter = 100,
                  run = maxiter,
                  maxFitness = Inf,
                  names = NULL,
                  suggestions = NULL,
                  parallel = FALSE,
                  monitor = if (interactive()) nsgaMonitor else FALSE,
                  summary = FALSE,
                  seed = NULL)
{

  call <- match.call()

  type <- match.arg(type, choices = eval(formals(rnsga2)$type))

  normalization <- match.arg(normalization, choices = eval(formals(rnsga2)$normalization))

  callArgs <- list(...)

  callArgs$strategy <- NULL

  if (!is.function(population))
    population <- get(population)
  if (!is.function(selection))
    selection <- get(selection)
  if (!is.function(crossover))
    crossover <- get(crossover)
  if (!is.function(mutation))
    mutation <- get(mutation)

  if (is.null(nObj)) {
    stop("Please, define the objective number (nObj)")
  } else {
    if (!is.numeric(nObj) | (nObj%%1!=0)) {
      stop("Objective number (nObj) is a character or is not an integer.")
    }
  }

  if (missing(fitness)) {
    stop("A fitness function must be provided")
  }
  if (!is.function(fitness)) {
    stop("A fitness function must be provided")
  }
  if (popSize < 10) {
    warning("The population size is less than 10.")
  }
  if (maxiter < 1) {
    stop("The maximum number of iterations must be at least 1.")
  }
  if (pcrossover < 0 | pcrossover > 1) {
    stop("Probability of crossover must be between 0 and 1.")
  }
  if (is.numeric(pmutation)) {
    if (pmutation < 0 | pmutation > 1) {
      stop("If numeric probability of mutation must be between 0 and 1.")
    } else if (!is.function(population)) {
      stop("pmutation must be a numeric value in (0,1) or a function.")
    }
  }

  if (missing(lower) & missing(upper) & missing(nBits)) {
    stop("A lower and upper range of values (for 'real-valued' or 'permutation') or nBits (for 'binary') must be provided!")
  }

  # if (is.null(nObj)) {
  #   nObj <- ncol(fitness(matrix(10000, ncol = 100, nrow = 100)))
  # }

  switch(type, binary = {
    nBits <- as.vector(nBits)[1]
    lower <- upper <- NA
    nvars <- nBits
    if (is.null(names)) names <- paste0("x", 1:nvars)
  }, `real-valued` = {
    lnames <- names(lower)
    unames <- names(upper)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
    nBits <- NA
    if (length(lower) != length(upper))
      stop("lower and upper must be vector of the same length")
    nvars <- length(upper)
    if (is.null(names) & !is.null(lnames)) names <- lnames
    if (is.null(names) & !is.null(unames)) names <- unames
    if (is.null(names))
      names <- paste0("x", 1:nvars)
  }, permutation = {
    lower <- as.vector(lower)[1]
    upper <- as.vector(upper)[1]
    nBits <- NA
    nvars <- length(seq.int(lower, upper))
    if (is.null(names))
      names <- paste0("x", 1:nvars)
  })

  if (is.null(suggestions)) {
    suggestions <- matrix(nrow = 0, ncol = nvars)
  } else {
    if (is.vector(suggestions)) {
      if (nvars > 1)
        suggestions <- matrix(suggestions, nrow = 1)
      else
        suggestions <- matrix(suggestions, ncol = 1)
    } else {
      suggestions <- as.matrix(suggestions)
    }
    if (nvars != ncol(suggestions))
      stop("Provided suggestions (ncol) matrix do not match number of variables of the problem")
  }

  # check monitor arg
  if (is.logical(monitor)) {
    if (monitor)
      monitor <- nsgaMonitor
  }
  if (is.null(monitor))
    monitor <- FALSE

  # Start parallel computing (if needed)
  if(is.logical(parallel)){
    if(parallel) {
      parallel <- startParallel(parallel)
      stopCluster <- TRUE
    } else {
      parallel <- stopCluster <- FALSE
    }
  }else {
    stopCluster <- if(inherits(parallel, "cluster")) FALSE else TRUE
    parallel <- startParallel(parallel)
  }
  on.exit(if(parallel & stopCluster)
    stopParallel(attr(parallel, "cluster")))
  # define operator to use depending on parallel being TRUE or FALSE
  `%DO%` <- if(parallel && requireNamespace("doRNG", quietly = TRUE)){
    doRNG::`%dorng%` } else if (parallel){ foreach::`%dopar%` } else { foreach::`%do%` }

  # set seed for reproducibility
  if (!is.null(seed))
    set.seed(seed)

  i. <- NULL  #dummy to trick R CMD check

  Fitness <- matrix(NA, nrow = popSize, ncol = nObj)

  fitnessSummary <- vector("list", maxiter)

  # Creacion del objetivo tipo nsga
  object <- new("rnsga2",
                call = call,
                type = type,
                lower = lower,
                upper = upper,
                nBits = nBits,
                names = if (is.null(names))
                  character()
                else names,
                popSize = popSize,
                front = matrix(),
                f = list(),
                iter = 0,
                run = 1,
                maxiter = maxiter,
                suggestions = suggestions,
                population = matrix(),
                pcrossover = pcrossover,
                pmutation = if (is.numeric(pmutation))
                  pmutation
                else NA,
                crowdingDistance = c(),
                fitness = Fitness,
                reference_points = reference_dirs,
                extreme_points = matrix(),
                smin =  rep(NA, nObj),
                summary = fitnessSummary)

    # Generate initial population
    if (maxiter == 0)
      return(object)

    p_fit <- q_fit <- matrix(NA_real_, nrow = popSize, ncol = nObj)
    switch(type,
      binary = {
        Pop <- P <- Q <- matrix(NA_real_, nrow = popSize, ncol = nBits)
      },
      `real-valued` = {
        Pop <- P <- Q <- matrix(NA_real_, nrow = popSize, ncol = nvars)
      },
      permutation = {
        Pop <- P <- Q <- matrix(NA_real_, nrow = popSize, ncol = nvars)
      }
    )

    ng <- min(nrow(suggestions), popSize)

    if (ng > 0) {
      Pop[1:ng, ] <- suggestions
    }
    if (popSize > ng) {
      Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - ng), ]
    }
    object@population <- Pop

    if(!parallel) {
      for (i in seq_len(popSize)) {
        if (is.na(Fitness[i])) {
          fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
          Fitness[i, ] <- fit
        }
      }
    } else {
      Fitness <- foreach(i. = seq_len(popSize), .combine = "rbind") %DO%
        { if(is.na(Fitness[i.]))
          do.call(fitness, c(list(Pop[i.,]), callArgs))
          else
            Fitness[i.,]
        }
    }

    object@population <- P <- Pop
    object@fitness <- p_fit <- Fitness

    #First Non-dominated Ranking
    out <- non_dominated_fronts(object)
    object@f <- out$fit
    object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
    # object@crowdingDistance <- c() Crowding measure with the smallest distance to reference points

    for (iter in seq_len(maxiter)) {
      object@iter <- iter

      #Selection Operator
      if (is.function(selection)) {
        sel <- selection(object, nObj)
        Pop <- sel$population
        Fitness <- sel$fitness
      } else {
        sel <- sample(1:popSize, size = popSize, replace = TRUE)
        Pop <- object@population[sel, ]
        Fitness <- object@fitness[sel, ]
      }
      object@population <- Pop
      object@fitness <- Fitness

      # Cross Operator
      if (is.function(crossover) & pcrossover > 0) {
        nmating <- floor(popSize / 2)
        mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)
        for (i in seq_len(nmating)) {
          if (pcrossover > runif(1)) {
            parents <- mating[i, ]
            Crossover <- crossover(object, parents)
            Pop[parents, ] <- Crossover$children
            Fitness[parents, ] <- Crossover$fitness
          }
        }
      }
      object@population <- Pop
      object@fitness <- Fitness

      #Mutation Operator
      pm <- if (is.function(pmutation)) {
        pmutation(object)
      } else {pmutation}
      if (is.function(mutation) & pm > 0) {
        for (i in seq_len(popSize)) {
          if (pm > runif(1)) {
            Mutation <- mutation(object, i)
            Pop[i, ] <- Mutation
            Fitness[i, ] <- NA
          }
        }
      }
      object@population <- Q <- Pop
      object@fitness <- q_fit <- Fitness

      #Evaluate Fitness
      if(!parallel) {
        for (i in seq_len(popSize)) {
          if (is.na(Fitness[i])) {
            fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
            Fitness[i, ] <- fit
          }
        }
      } else {
        Fitness <- foreach(i. = seq_len(popSize), .combine = "rbind") %DO%
          { if(is.na(Fitness[i.]))
            do.call(fitness, c(list(Pop[i.,]), callArgs))
            else
              Fitness[i.,]
          }
      }

      object@population <- Q <- Pop
      object@fitness <- q_fit <- Fitness

      #R = P U Q
      object@population <- Pop <- rbind(P, Q)
      object@fitness <- rbind(p_fit, q_fit)

      out <- non_dominated_fronts(object)
      object@f <- out$fit
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

      #R-NSGA-II Operator
      cd <- modifiedCrowdingDistance(object, ...)
      object@crowdingDistance <- cd$survivors
      object@reference_points <- cd$reference_points
      object@smin <- cd$indexmin
      rm(cd)

      # Sorted population and fitness by front and crowding distance
      populationsorted <- object@population[object@crowdingDistance, ]
      fitnesssorted <- object@fitness[object@crowdingDistance, ]

      # Select de first N element
      object@population <- P <- Pop <- populationsorted
      object@fitness <- p_fit <- fitnesssorted

      out <- non_dominated_fronts(object)
      object@f <- out$fit
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

      # ------------------------------------------------------------------------
      if (summary == TRUE) {
        fitnessSummary[[iter]] <- progress(object, callArgs)
        object@summary <- fitnessSummary
      } else {
        object@summary <- list(NULL)
      }

      # Plot front non-dominated by iteration
      if (is.function(monitor)) {
        monitor(object = object, number_objective = nObj)
      }

      if (max(Fitness, na.rm = TRUE) >= maxFitness)
        break
      if (object@iter == maxiter)
        break
    }

    solution <- object

    return(solution)
}


modifiedCrowdingDistance <- function(object,
                                     epsilon=0.001,
                                     weights = NULL,
                                     normalization = "front",
                                     extreme_points_as_ref_dirs = FALSE, ...) {
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
    smin <-  ps$smin
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
      rank_by_distance <- apply(apply(distance_to_ref_points[fronts[[i]],], 2, order), 2, order)
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

        group = not_selected[which(dist < epsilon)][1]

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

calc_norm_pref_distance <- function(fitness,
                                    ref_points,
                                    weight,
                                    ideal_point,
                                    nadir_point){
  if(!is.matrix(ref_points)){
    ref_points <- t(ref_points)
  }
  if(!is.matrix(fitness)){
    fitness <- t(fitness)
  }

  D <- matrix(rep(fitness,
                  each=nrow(ref_points)),
              ncol = ncol(ref_points),
              byrow = FALSE) - matrix(rep(t(ref_points),nrow(fitness)),
                                      ncol = ncol(fitness),
                                      byrow = TRUE)
  denom <- nadir_point - ideal_point # New
  denom[which(denom == 0)] <- 1 * 10^(-12) # New
  N <- ((sweep(D, 2, denom, FUN = "/"))^2) * weight
  N <- sqrt(apply(N, 1, sum) * length(weight))

  return(matrix(N, nrow(fitness), nrow(ref_points), byrow = TRUE))

}

# setClass(Class = "rnsga2",
#          slots = list(crowdingDistance = "numberOrNAOrMatrix",
#                       reference_points = "numberOrNAOrMatrix",
#                       extreme_points = "numberOrNAOrMatrix",
#                       smin = "numberOrNAOrMatrix"),
#          contains = "nsga2")



# setClass(Class = "rnsga2",
#          slots = list(crowdingDistance = "numberOrNAOrMatrix",
#                       reference_points = "numberOrNAOrMatrix",
#                       extreme_points = "numberOrNAOrMatrix",
#                       smin = "numberOrNAOrMatrix"),
#          contains = "nsga2")
#
# object <- new("rnsga2",
#               type = "real-valued",
#               lower = c(0,0),
#               upper = c(1,1),
#               popSize = 25,
#               front = matrix(),
#               f = list(),
#               iter = 0,
#               run = 1,
#               maxiter = 100,
#               population = matrix(),
#               pcrossover = 0.8,
#               pmutation = 0.2,
#               crowdingDistance = c(),
#               reference_points = ref_points,
#               fitness = fit_2)
#
# object <- new("rnsga2",
#               call = call,
#               type = type,
#               lower = lower,
#               upper = upper,
#               nBits = nBits,
#               names = if (is.null(names))
#                 character()
#               else names,
#               popSize = popSize,
#               front = matrix(),
#               f = list(),
#               iter = 0,
#               run = 1,
#               maxiter = maxiter,
#               suggestions = suggestions,
#               population = matrix(),
#               pcrossover = pcrossover,
#               pmutation = if (is.numeric(pmutation))
#                 pmutation
#               else NA,
#               crowdingDistance = matrix(),
#               fitness = Fitness,
#               reference_points = ref_points,
#               reference_points = reference_dirs,
#               extreme_points = matrix(),
#               smin =  rep(NA, nObj),
#               summary = fitnessSummary)
#
#
#
#
# object@fitness
# nObj <- ncol(object@fitness)
# fronts <- object@f
# nFront <- length(object@f)
# popSize <- object@popSize
# reference_points <- object@reference_points
#
#
# modifiedCrowdingDistance(fit, fronts, popSize, ref_points)

# n_remaining <- popSize
# survivors <- c()
# weights <- rep(1/n_obj, n_obj)
# distance_to_ref_points <- calc_norm_pref_distance(fitness,
#                                                   reference_points,
#                                                   weights,
#                                                   ideal_point,
#                                                   nadir_point)
# for (i in seq_len(nFront)) {
#   n_remaining <- popSize - length(survivors)
#
#   rank_by_distance <- apply(apply(distance_to_ref_points[fronts[[i]],], 2, order), 2, order)
#   ref_point_of_best_rank <- apply(rank_by_distance, 1, which.min)
#   ranking <- diag(rank_by_distance[seq_len(length(fronts[[i]])), ref_point_of_best_rank])
#
#   if (length(fronts[[i]]) <= n_remaining){
#     crowding <- ranking
#     I <- seq_len(length(fronts[[i]]))
#   } else{
#     dist_to_others <- calc_norm_pref_distance(fitness=fitness[fronts[[i]],],
#                                               ref_points=fitness[fronts[[i]],],
#                                               weight=weight,
#                                               ideal_point=ideal_point,
#                                               nadir_point=nadir_point)
#     diag(dist_to_others) <- Inf
#     crowding <- rep(NA_real_,length(fronts[[i]]))
#     not_selected <- order(ranking)
#
#     while (length(not_selected) > 0) {
#       idx <- not_selected[1]
#       crowding[idx] <- ranking[idx]
#       to_remove <- c(idx)
#
#       dist <- dist_to_others[idx, not_selected]
#
#       group <- not_selected[which(dist < epsilon)[1]]
#
#       if (!is.na(which(dist < epsilon)[1])){
#         crowding[group] <- ranking[group] + round(length(fronts[[i]])/2)
#
#         # remove group from not_selected array
#         to_remove <- c(to_remove, group)
#
#
#       }
#       not_selected <- not_selected[which(not_selected != to_remove)]
#
#     }
#     I <- order(crowding)[1:n_remaining]
#
#   }
#   # object@crowding <- rbind(crowding, object@crowding)
#   survivors <- c(survivors,front[I])
# }
#
#
# zaux = znad - zstar;
# zrn = (zr - zstar)/zaux;
# sweep(fitness, 2, zstar)/matrix(rep(zaux, nrow(fitness)),ncol=3, byrow = TRUE)

# get_extreme_points_c <- function(fitness,
#                                  ideal_point,
#                                  extreme_points = NULL){
#   nObj <- ncol(fitness)
#   w <- diag(1, nObj)
#   w[which(w == 0)] <- 1e-06
#   Fit <- fitness
#   if (!is.null(extreme_points)){
#     Fit <- rbind(extreme_points, F)
#   }
#   Fit_ <- Fit - ideal_point
#   Fit_[which(Fit_ < 0.001)] <- 0
#
#   Fit_asf <- ma
#
# }

# modifiedCrowdingDistance
# function(object,
#          epsilon=0.001,
#          weights = NULL,
#          normalization = "front",
#          extreme_points_as_ref_dirs = FALSE) {
#   fitness <- object@fitness
#   population <- object@population
#   nObj <- ncol(object@fitness)
#   fronts <- object@f
#   nFront <- length(object@f)
#   popSize <- object@popSize
#   reference_points <- object@reference_points
#
#   if (is.null(weights)) {
#     weights <- rep((1/nObj),nObj)
#   }
#
#   ideal_point <- rep(Inf, nObj)
#   nadir_point <- rep(-Inf, nObj)
#
#   if (normalization == "ever") {
#     ideal_point <- apply(rbind(ideal_point,fitness), 2, min)
#     nadir_point <- apply(rbind(ideal_point,fitness), 2, max)
#   } else if (normalization == "front") {
#     if (length(fronts[[1]]) > 1) {
#       ideal_point <- apply(fitness[fronts[[1]],], 2, min)
#       nadir_point <- apply(fitness[fronts[[1]],], 2, max)
#     }
#   } else if (normalization == "no"){
#     ideal_point <- rep(1,nObj)
#     nadir_point <- rep(0,nObj)
#
#   }
#
#   if (extreme_points_as_ref_dirs){
#     ps <- PerformScalarizing(population = population[unlist(fronts), ],
#                              fitness = fitness[unlist(fronts), ],
#                              smin = object@smin,
#                              extreme_points = reference_points,
#                              ideal_point = ideal_point)
#     reference_points <- rbind(reference_points, ps$extremepoint)
#   }
#
#   n_remaining <- popSize
#   survivors <- c()
#
#   distance_to_ref_points <- calc_norm_pref_distance(fitness=fitness,
#                                                     ref_points=reference_points,
#                                                     weight=weights,
#                                                     ideal_point=ideal_point,
#                                                     nadir_point=nadir_point)
#
#   for (i in seq_len(nFront)) {
#     n_remaining <- popSize - length(survivors)
#     if(n_remaining==0) break
#     if(length(fronts[[i]]) > 1){
#       rank_by_distance <- apply(apply(distance_to_ref_points[fronts[[i]],], 2, order), 2, order)
#       ref_point_of_best_rank <- apply(rank_by_distance, 1, which.min)
#     }else{
#       rank_by_distance <-  order(order(distance_to_ref_points[fronts[[i]],]))
#       ref_point_of_best_rank <- which.min(distance_to_ref_points)
#     }
#     ranking <- diag(rank_by_distance[seq_len(length(fronts[[i]])), ref_point_of_best_rank])
#
#     if (length(fronts[[i]]) < n_remaining){
#       crowding <- ranking
#       I <- seq_len(length(fronts[[i]]))
#     } else{
#       dist_to_others <- calc_norm_pref_distance(fitness=fitness[fronts[[i]],],
#                                                 ref_points=fitness[fronts[[i]],],
#                                                 weight=weights,
#                                                 ideal_point=ideal_point,
#                                                 nadir_point=nadir_point)
#       diag(dist_to_others) <- Inf
#       crowding <- rep(NA_real_, length(fronts[[i]]))
#       not_selected <- order(ranking)
#
#       while (length(not_selected) > 0) {
#         idx <- not_selected[1]
#         crowding[idx] <- ranking[idx]
#         to_remove <- c(idx)
#
#         dist <- dist_to_others[idx, not_selected]
#
#         group = not_selected[which(dist < epsilon)][1]
#
#         if (!is.na(group)){
#           if (length(group)){
#             crowding[group] <- ranking[group] + round(length(fronts[[i]]) / 2)
#
#             # remove group from not_selected array
#             to_remove <- c(to_remove, group)
#
#
#           }
#         }
#         not_selected <- not_selected[which(not_selected != to_remove)]
#
#       }
#       I <- order(crowding)[1:n_remaining]
#
#     }
#     survivors <- c(survivors,fronts[[i]][I])
#
#   }
#   out <- list(survivors = survivors,
#               smin = ps$indexmin,
#               reference_points = reference_points)
#   return(out)
# }
#

# Test
#
#
# fit <- rbind(c(0.66780144, 0.56040272, 0.53869672),
#                  c(0.22400583, 0.67217154, 0.67665191),
#                  c(0.28343703, 0.22172591, 0.94241118),
#                  c(0.1154173 , 0.55496445, 0.99940443),
#                  c(0.81196831, 0.75501992, 0.71940294),
#                  c(0.3257302 , 0.79813577, 0.83363384),
#                  c(0.89800515, 0.90915025, 0.75975222),
#                  c(0.89193585, 0.22773071, 0.26137382),
#                  c(0.42234651, 0.71074372, 0.59098554),
#                  c(0.86140305, 0.38865218, 0.93676129),
#                   c(0.6044384 , 0.06157129, 0.8777875),
#                   c(0.68970106, 0.89612004, 0.91304825),
#                   c(0.80986242, 0.00504261, 0.17546453),
#                   c(0.46528027, 0.1320064 , 0.5372177 ),
#                   c(0.85764736, 0.09979807, 0.52751029),
#                   c(0.53606087, 0.80429671, 0.71650414),
#                   c(0.1329895 , 0.83908972, 0.13128153),
#                   c(0.08946272, 0.13789929, 0.68126607),
#                   c(0.71132944, 0.38014792, 0.98091205),
#                   c(0.40109313, 0.38169696, 0.60444014))
#
# weights <- rep(1/3,3)
#
#
# ref_points <- rbind(c(0.5,0.2,0.8),c(0.1,0.6,0.3))
