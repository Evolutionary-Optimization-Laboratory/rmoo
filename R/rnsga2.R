#' Reference Point Based Non-Dominated Sorting in Genetic Algorithms II
#'
#' Minimization of a fitness function using reference point based non-dominated
#' sorting genetic algorithms - II (R-NSGA-IIs). Multiobjective evolutionary algorithms
#'
#' R-NSGA-II is a meta-heuristic proposed by K. Deb and J. Sundar in 2006.
#' It is a modification of NSGA-II based on reference points in which the
#' decision-maker supplies one or more preference points and a weight vector
#' that will guide the solutions towards regions desired by the user.
#'
#'
#' @param type the type of genetic algorithm to be run depending on the nature
#' of decision variables. Possible values are:
#' \describe{
#'     \item{'binary'}{for binary representations of decision variables.}
#'     \item{'real-valued'}{for optimization problems where the decision
#'     variables are floating-point representations of real numbers.}
#'     \item{'permutation'}{for problems that involves reordering of a list of
#'     objects.}
#' }
#'
#' @param fitness the fitness function, any allowable R function which takes as
#' input an individual string representing a potential solution, and returns a
#' numerical value describing its 'fitness'.
#' @param ... additional arguments to be passed to the fitness function. This
#' allows to write fitness functions that keep some variables fixed during the
#' search
#' @param lower a vector of length equal to the decision variables providing the
#' lower bounds of the search space in case of real-valued or permutation
#' encoded optimizations.
#' @param upper a vector of length equal to the decision variables providing the
#' upper bounds of the search space in case of real-valued or permutation
#' encoded optimizations.
#' @param nBits a value specifying the number of bits to be used in binary
#' encoded optimizations
#' @param population an R function for randomly generating an initial population.
#' See [rmoo_Population()] for available functions.
#' @param selection an R function performing selection, i.e. a function which
#' generates a new population of individuals from the current population
#' probabilistically according to individual fitness. See [rmoo_Selection()]
#' for available functions.
#' @param crossover an R function performing crossover, i.e. a function which
#' forms offsprings by combining part of the genetic information from their
#' parents. See [rmoo_Crossover()] for available functions.
#' @param mutation an R function performing mutation, i.e. a function which
#' randomly alters the values of some genes in a parent chromosome.
#' See [rmoo_Mutation()] for available functions.
#' @param reference_dirs Function to generate reference points using Das and
#' Dennis approach or matrix with supplied reference points.
#' @param epsilon controls the extent of obtained solutions by grouping all
#' solutions that have a normalized difference sum in objective values of epsilon or less.
#' @param normalization of the ideal points and nadir. They can be:
#' \describe{
#'     \item{'ever'}{.}
#'     \item{'front'}{.}
#'     \item{'no'}{.}
#' }
#'
#' @param extreme_points_as_ref_dirs flag to use extreme points as reference points.
#' @param weights vector specifies the importance of one objective function over
#' the other, by default all objectives have equal weights.
#' @param popSize the population size.
#' @param nObj number of objective in the fitness function.
#' @param pcrossover the probability of crossover between pairs of chromosomes.
#' Typically this is a large value and by default is set to 0.8.
#' @param pmutation the probability of mutation in a parent chromosome. Usually
#' mutation occurs with a small probability, and by default is set to 0.1.
#' @param maxiter the maximum number of iterations to run before the NSGA search
#' is halted.
#' @param run the number of consecutive generations without any improvement in
#' the best fitness value before the NSGA is stopped
#' @param maxFitness the upper bound on the fitness function after that the NSGA
#' search is interrupted.
#' @param names a vector of character strings providing the names of decision
#' variables.
#' @param suggestions a matrix of solutions strings to be included in the
#' initial population. If provided the number of columns must match the number
#' of decision variables.
#' @param parallel An optional argument which allows to specify if the NSGA-II
#' should be run sequentially or in parallel.
#' @param monitor a logical or an R function which takes as input the current
#' state of the nsga-class object and show the evolution of the search. By
#' default, for interactive sessions the function rmooMonitor prints the average
#' and best fitness values at each iteration. If set to plot these information
#' are plotted on a graphical device. Other functions can be written by the user
#' and supplied as argument. In non interactive sessions, by default
#' monitor = FALSE so any output is suppressed.
#' @param summary If there will be a summary generation after generation.
#' @param seed an integer value containing the random number generator state.
#' This argument can be used to replicate the results of a NSGA search. Note
#' that if parallel computing is required, the doRNG package must be installed.
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
#' @seealso [nsga()], [nsga2()], [nsga3()]
#'
#' @return Returns an object of class rnsga2-class. See [rnsga2-class] for a
#' description of available slots information.
#'
#' @examples
#' #Example
#' #Two Objectives - Real Valued
#' zdt1 <- function (x) {
#'  if (is.null(dim(x))) {
#'    x <- matrix(x, nrow = 1)
#'  }
#'  n <- ncol(x)
#'  g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#'  return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
#' }
#'
#' #Define the reference points
#' reference_points = rbind(c(0.2, 0.8), c(0.8, 0.2), c(0.4, 0.5))
#'
#' #Not run:
#' \dontrun{
#' result <- rnsga2(type = "real-valued",
#'                 fitness = zdt1,
#'                 lower = c(0,0),
#'                 upper = c(1,1),
#'                 reference_dirs = reference_points,
#'                 popSize = 100,
#'                 nObj = 2,
#'                 monitor = FALSE,
#'                 maxiter = 500,
#'                 seed = 45)
#' }
#'
#' #Example 2
#' #Three Objectives - Real Valued
#' dtlz1 <- function (x, nobj = 3){
#'     if (is.null(dim(x))) {
#'         x <- matrix(x, 1)
#'     }
#'     n <- ncol(x)
#'     y <- matrix(x[, 1:(nobj - 1)], nrow(x))
#'     z <- matrix(x[, nobj:n], nrow(x))
#'     g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 * pi * (z - 0.5))))
#'     tmp <- t(apply(y, 1, cumprod))
#'     tmp <- cbind(t(apply(tmp, 1, rev)), 1)
#'     tmp2 <- cbind(1, t(apply(1 - y, 1, rev)))
#'     f <- tmp * tmp2 * 0.5 * (1 + g)
#'     return(f)
#' }
#'
#' #Define the reference points
#' reference_points <- rbind(c(1.0, 0.5, 0.0), c(0.0, 0.5, 1.0), c(0.5, 0.5, 0.5))
#'
#' #Not run:
#' \dontrun{
#' result <- rnsga2(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = c(0,0,0),
#'                 upper = c(1,1,1),
#'                 reference_dirs = reference_points,
#'                 popSize = 92,
#'                 nObj = 3,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#'
#' @export
rnsga2 <- function(type = c("binary", "real-valued", "permutation"),
                  fitness, ...,
                  lower, upper, nBits,
                  population = rmooControl(type)$population,
                  selection = rmooControl(type)$selection,
                  crossover = rmooControl(type)$crossover,
                  mutation = rmooControl(type)$mutation,
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
                  monitor = if (interactive()) rmooMonitor else FALSE,
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

  if (is.null(reference_dirs)) {
    stop("Please, define the reference points (reference_dirs)")
  } else {
    if ((ncol(reference_dirs) != nObj) & !is.matrix(reference_dirs)) {
      stop("The provided reference points must be a matrix and have the same
    columns as objective number function")
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
      monitor <- rmooMonitor
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
      cd <- modifiedCrowdingDistance(object, epsilon, weights, normalization, extreme_points_as_ref_dirs)
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
        monitor(object = object, callArgs)
        # monitor(object = object, number_objective = nObj)
      }

      if (max(Fitness, na.rm = TRUE) >= maxFitness)
        break
      if (object@iter == maxiter)
        break
    }

    solution <- object

    return(solution)
}

## R-NSGA-II Bare Process
# @export
r_nsga_ii <- function(object, epsilon, weights, normalization, extreme_points_as_ref_dirs) {
  cd <- modifiedCrowdingDistance(object, epsilon, weights, normalization, extreme_points_as_ref_dirs)
  object@crowdingDistance <- cd$survivors

  object@population <- P <- Pop <- object@population[object@crowdingDistance, ]
  object@fitness <- p_fit <- object@fitness[object@crowdingDistance, ]

  out <- non_dominated_fronts(object)
  object@f <- out$fit
  object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

  cd <- modifiedCrowdingDistance(object, epsilon, weights, normalization, extreme_points_as_ref_dirs)
  object@crowdingDistance <- cd$survivors

  out <- list(object = object,
              p_pop = Pop,
              p_fit = p_fit)

  return(out)
}

# @export
# @rdname progress-methods
# @aliases progress,rnsga2-method
#setMethod("progress", "rnsga2", .rnsga2.progress)

# @export
#' @rdname plot-methods
#' @aliases plot,rnsga2-method
setMethod("plot", signature(x="rnsga2", y="missing"), .get.plotting)





