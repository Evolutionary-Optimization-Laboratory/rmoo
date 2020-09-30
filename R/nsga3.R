#' Non-Dominated Sorting in Genetic Algorithms III
#'
#' Minimization of a fitness function using non-dominated sorting genetic algorithms - III (NSGA-IIIs).
#' Multiobjective evolutionary algorithms
#'
#' The Non-dominated genetic algorithms III is a meta-heuristic proposed by K. Deb and H. Jain in 2013.
#' The purpose of the algorithms is to find an efficient way to optimize multi-objectives functions (more than three).
#'
#' @param type the type of genetic algorithm to be run depending on the nature of decision variables.
#' Possible values are:
#' \describe{
#' 	\item{\code{"binary"}}{for binary representations of decision variables.}
#'	\item{\code{"real-valued"}}{for optimization problems where the decision variables are floating-point representations of real numbers.}
#' 	\item{\code{"permutation"}}{for problems that involves reordering of a list of objects.}
#' }
#'
#' @param fitness the fitness function, any allowable R function which takes as input an individual string representing a potential solution, and returns a numerical value describing its “fitness”.
#' @param ... additional arguments to be passed to the fitness function. This allows to write fitness functions that keep some variables fixed during the search
#' @param lower a vector of length equal to the decision variables providing the lower bounds of the search space in case of real-valued or permutation encoded optimizations. Formerly this argument was named min; its usage is allowed but deprecated.
#' @param upper a vector of length equal to the decision variables providing the upper bounds of the search space in case of real-valued or permutation encoded optimizations. Formerly this argument was named max; its usage is allowed but deprecated.
#' @param nBits a value specifying the number of bits to be used in binary encoded optimizations
#' @param population an R function for randomly generating an initial population. See [nsga_Population()] for available functions.
#' @param selection an R function performing selection, i.e. a function which generates a new population of individuals from the current population probabilistically according to individual fitness. See [nsga_Selection()] for available functions.
#' @param crossover an R function performing crossover, i.e. a function which forms offsprings by combining part of the genetic information from their parents. See [nsga_Crossover()] for available functions.
#' @param mutation an R function performing mutation, i.e. a function which randomly alters the values of some genes in a parent chromosome. See [nsga_Mutation()] for available functions.
#' @param popSize the population size.
#' @param nObj number of objective in the fitness function.
#' @param n_partitions Partition number of generated reference points
#' @param pcrossover the probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8.
#' @param pmutation the probability of mutation in a parent chromosome. Usually mutation occurs with a small probability, and by default is set to 0.1.
#' @param reference_dirs Function to generate reference points using Das and Dennis approach or matrix with supplied reference points.
#' @param maxiter the maximum number of iterations to run before the NSGA search is halted.
#' @param run the number of consecutive generations without any improvement in the best fitness value before the NSGA is stopped
#' @param maxFitness the upper bound on the fitness function after that the NSGA search is interrupted.
#' @param names a vector of character strings providing the names of decision variables.
#' @param suggestions a matrix of solutions strings to be included in the initial population. If provided the number of columns must match the number of decision variables.
#' @param monitor a logical or an R function which takes as input the current state of the nsga-class object and show the evolution of the search. By default, for interactive sessions the function nsgaMonitor prints the average and best fitness values at each iteration. If set to plot these information are plotted on a graphical device. Other functions can be written by the user and supplied as argument. In non interactive sessions, by default monitor = FALSE so any output is suppressed.
#' @param seed an integer value containing the random number generator state. This argument can be used to replicate the results of a NSGA search. Note that if parallel computing is required, the doRNG package must be installed.
#'
#' @author Francisco Benitez
#' \email{benitez.fj@@hotmail.com}
#'
#' @references K. Deb and H. Jain, "An Evolutionary Many-Objective Optimization Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I: Solving Problems With Box Constraints," in IEEE Transactions on Evolutionary Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014, doi: 10.1109/TEVC.2013.2281535.
#'
#' Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
#' \url{https://www.jstatsoft.org/v53/i04/}.
#'
#' @seealso [nsga()], [nsga2()]
#'
#' @return Returns an object of class nsga3-class. See [nsga3-class] for a description of available slots information.
#' @export
nsga3 <- function(type = c("binary", "real-valued", "permutation"),
    fitness, ...,
    lower, upper, nBits,
    population = nsgaControl(type)$population,
    selection = nsgaControl(type)$selection,
    crossover = nsgaControl(type)$crossover,
    mutation = nsgaControl(type)$mutation,
    popSize = 50,
    nObj = ncol(fitness(matrix(10000, ncol = 100, nrow = 100))),
    n_partitions,
    pcrossover = 0.8,
    pmutation = 0.1,
    reference_dirs = generate_reference_points,
    maxiter = 100,
    run = maxiter,
    maxFitness = Inf,
    names = NULL,
    suggestions = NULL,
    monitor = if (interactive()) nsgaMonitor else FALSE,
    seed = NULL)
{
    call <- match.call()

    type <- match.arg(type, choices = eval(formals(nsga3)$type))

    algorithm <- "NSGA-III"

    callArgs <- list(...)

    if (!is.function(population))
      population <- get(population)
    if (!is.function(selection))
      selection <- get(selection)
    if (!is.function(crossover))
      crossover <- get(crossover)
    if (!is.function(mutation))
      mutation <- get(mutation)

    if (!is.function(reference_dirs) & !is.matrix(reference_dirs)) {
      stop("A Determination of Reference Points function or matrix must be provided ")
    }

    if (is.function(reference_dirs) & is.null(popSize)) {
      popSize <- nrow(reference_dirs(nObj, n_partitions))
    } else {
      if (is.matrix(reference_dirs) & is.null(popSize)) {
        popSize <- nrow(reference_dirs)
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
      }
      else if (!is.function(population)) {
        stop("pmutation must be a numeric value in (0,1) or a function.")
      }
    }

    if (missing(lower) & missing(upper) & missing(nBits)) {
      stop("A lower and upper range of values (for 'real-valued' or 'permutation') or nBits (for 'binary') must be provided!")
    }

    if (is.null(nObj)) {
      nObj <- ncol(fitness(matrix(10000, ncol = 100, nrow = 100)))
    }

    #Generate reference points, otherwise, assign the provided matrix
    if (is.function(reference_dirs)) {
      ref_dirs <- reference_dirs(nObj, n_partitions)
    } else {
      ref_dirs <- reference_dirs
    }

    if (ncol(ref_dirs) != nObj) {
      stop("Dimensionality of reference points must be equal to the number of objectives")
    }

    switch(type,
      binary = {
        nBits <- as.vector(nBits)[1]
        lower <- upper <- NA
        nvars <- nBits
        if (is.null(names)) names <- paste0("x", 1:nvars)
      },
      `real-valued` = {
        lnames <- names(lower)
        unames <- names(upper)
        lower <- as.vector(lower)
        upper <- as.vector(upper)
        nBits <- NA
        if (length(lower) != length(upper))
          stop("lower and upper must be vector of the same length")
        if ((length(lower) != nObj) & (length(upper) != nObj))
          stop("The lower and upper limits must be vector of the same number of objectives")
        nvars <- length(upper)
        if (is.null(names) & !is.null(lnames))
          names <- lnames
        if (is.null(names) & !is.null(unames))
          names <- unames
        if (is.null(names))
          names <- paste0("x", 1:nvars)
      },
      permutation = {
        lower <- as.vector(lower)[1]
        upper <- as.vector(upper)[1]
        nBits <- NA
        nvars <- length(seq.int(lower, upper))
        if (is.null(names))
          names <- paste0("x", 1:nvars)
      }
    )

    if (is.null(suggestions)) {
      suggestions <- matrix(nrow = 0, ncol = nvars)
    } else {
      if (is.vector(suggestions)) {
        if (nvars > 1)
          suggestions <- matrix(suggestions, nrow = 1)
        else suggestions <- matrix(suggestions, ncol = 1)
      }
      else {
        suggestions <- as.matrix(suggestions)
      }
      if (nvars != ncol(suggestions))
        stop("Provided suggestions (ncol) matrix do not match number of variables of the problem")
    }

    # check monitor arg
    if (is.logical(monitor)) {
      if (monitor) monitor <- nsgaMonitor
    }
    if (is.null(monitor)) monitor <- FALSE

    # set seed for reproducibility
    if (!is.null(seed))
      set.seed(seed)

    i. <- NULL #dummy to trick R CMD check

    Fitness <- matrix(NA, nrow = popSize, ncol = nObj)

    fitnessSummary <- vector("list", maxiter)

    n_remaining <- popSize

    #Creacion del objetivo tipo nsga
    object <- new("nsga3",
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
        ideal_point = NA, #Agregar en nsga3-class
        worst_point = NA, #Agregar en nsga3-class
        smin = rep(NA, nObj),
        extreme_points = matrix(), #Agregar en nsga3-class
        worst_of_population = rep(NA, nObj), #Agregar en nsga3-class
        worst_of_front = rep(NA, nObj), #Agregar en nsga3-class
        nadir_point = rep(NA, nObj),
        pcrossover = pcrossover,
        pmutation = if (is.numeric(pmutation))
          pmutation
        else NA,
        reference_points = ref_dirs, #Agregar en nsga3-class
        fitness = Fitness,
        summary = fitnessSummary)

    #Generate initial population
    if (maxiter == 0)
      return(object)

    p_fit <- q_fit <- matrix(as.double(NA), nrow = popSize, ncol = nObj)
    switch(type,
      binary = {
        Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nBits)
      },
      `real-valued` = {
        Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nObj)
      },
      permutation = {
        Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
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

    for (i in seq_len(popSize)) {
      if (is.na(Fitness[i])) {
        fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
        Fitness[i, ] <- fit
      }
    }

    object@population <- P <- Pop
    object@fitness <- p_fit <- Fitness

    #First Non-dominated Ranking
    out <- non_dominated_fronts(object)
    object@f <- out$fit
    object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

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

      #Cross Operator
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
            Fitness[i,] <- NA
          }
        }
      }
      object@population <- Q <- Pop
      object@fitness <- q_fit <- Fitness

      #Evaluate Fitness
      for (i in seq_len(popSize)) {
        if (is.na(Fitness[i])) {
          fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
          Fitness[i,] <- fit
        }
      }

      object@population <- Q <- Pop
      object@fitness <- q_fit <- Fitness

      #R = P U Q
      object@population <- Pop <- rbind(P,Q)
      object@fitness <- rbind(p_fit, q_fit)

      #NSGA-III Operator
      ideal_point <- UpdateIdealPoint(object, nObj)
      worst_point <- UpdateWorstPoint(object, nObj)

      object@ideal_point <- ideal_point
      object@worst_point <- worst_point

      out <- non_dominated_fronts(object)
      con <- 0
      for (i in 1:length(out$fit)) {
        con <- con + length(out$fit[[i]])
        st <- i
        if(con >= object@popSize) break
      }
      object@f <- out$fit[1:st]
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
      rm(out)

      ps <- PerformScalarizing(object@population[unlist(object@f), ],
                               object@fitness[unlist(object@f), ],
                               object@smin,
                               object@extreme_points,
                               object@ideal_point)

      object@extreme_points <- ps$extremepoint
      object@smin <- ps$indexmin

      worst_of_population <- worst_of_front <- c()

      worst_of_population <- apply(object@fitness, 2, max)
      worst_of_front <- apply(object@fitness[object@f[[1]], ], 2, max)

      object@worst_of_population <- worst_of_population
      object@worst_of_front <- worst_of_front

      nadir_point <- get_nadir_point(object)

      object@nadir_point <- nadir_point

      I <- unlist(object@f)
      object@population <- object@population[I, ]
      object@front <-  object@front[I, ]
      object@fitness <- object@fitness[I, ]

      out <- non_dominated_fronts(object)
      object@f <- out$fit
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
      last_front <- out$fit[[max(length(out$fit))]]
      rm(out)

      outniches <- associate_to_niches(object)
      niche_of_individuals <- outniches$niches
      dist_to_niche <- outniches$distance
      rm(outniches)

      #Generate the next generation
      if (nrow(object@population) > popSize) {
        if (length(object@f) == 1) {
          until_last_front <- c()
          niche_count <- rep(0, nrow(object@reference_points))
          n_remaining <- popSize
        } else {
          until_last_front <- unlist(object@f[1:(length(object@f) - 1)])
          niche_count <- compute_niche_count(nrow(object@reference_points),
                                             niche_of_individuals[until_last_front])
          n_remaining <- popSize - length(until_last_front)
        }
        s_idx  <- niching(pop = object@population[last_front, ],
                          n_remaining = n_remaining,
                          niche_count = niche_count,
                          niche_of_individuals = niche_of_individuals[last_front],
                          dist_to_niche = dist_to_niche[last_front])
        survivors <- append(until_last_front, last_front[s_idx])
        object@population <- P <- Pop <- object@population[survivors, ]
        object@fitness <- p_fit <- object@fitness[survivors, ]
      }

      out <- non_dominated_fronts(object)
      object@f <- out$fit
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
      rm(out)

      fitnessSummary[[iter]] <- nsgaiiiSummary(object)
      object@summary <- fitnessSummary

      #Plot front non-dominated by iteration
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
