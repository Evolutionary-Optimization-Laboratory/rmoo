#' Non-Dominated Sorting in Genetic Algorithms
#'
#' Minimization of a fitness function using Non-Dominated Genetic algorithms
#' (NSGA). Local search using general-purpose optimisation algorithms can be
#' applied stochastically to exploit interesting regions.
#'
#' The Non-dominated genetic algorithms is a meta-heuristic proposed by
#' N. Srinivas and K. Deb in 1994. The purpose of the algorithms is to find an
#' efficient way to optimize multi-objectives functions (two or more).
#'
#' @param type the type of genetic algorithm to be run depending on the nature
#' of decision variables. Possible values are:
#' \describe{
#' 	\item{\code{"binary"}}{for binary representations of decision variables.}
#'	\item{\code{"real-valued"}}{for optimization problems where the decision
#'	variables are floating-point representations of real numbers.}
#' 	\item{\code{"permutation"}}{for problems that involves reordering of a list
#' 	of objects.}
#' }
#'
#' @param fitness the fitness function, any allowable R function which takes as
#' input an individual string representing a potential solution, and returns a
#' numerical value describing its “fitness”.
#' @param ... additional arguments to be passed to the fitness function. This
#' allows to write fitness functions that keep some variables fixed during the
#' search.
#' @param lower a vector of length equal to the decision variables providing the
#' lower bounds of the search space in case of real-valued or permutation
#' encoded optimizations.
#' @param upper a vector of length equal to the decision variables providing the
#' upper bounds of the search space in case of real-valued or permutation
#' encoded optimizations.
#' @param nBits a value specifying the number of bits to be used in binary
#' encoded optimizations.
#' @param population an R function for randomly generating an initial population.
#' See [nsga_Population()] for available functions.
#' @param selection an R function performing selection, i.e. a function which
#' generates a new population of individuals from the current population
#' probabilistically according to individual fitness.
#' See [nsga_Selection()] for available functions.
#' @param crossover an R function performing crossover, i.e. a function which
#' forms offsprings by combining part of the genetic information from
#' their parents. See [nsga_Crossover()] for available functions.
#' @param mutation an R function performing mutation, i.e. a function which
#' randomly alters the values of some genes in a parent chromosome.
#' See [nsga_Mutation()] for available functions.
#' @param popSize the population size.
#' @param nObj number of objective in the fitness function.
#' @param dshare the maximun phenotypic distance allowed between any two
#' individuals to become members of a niche.
#' @param pcrossover the probability of crossover between pairs of chromosomes.
#' Typically this is a large value and by default is set to 0.8.
#' @param pmutation the probability of mutation in a parent chromosome. Usually
#' mutation occurs with a small probability, and by default is set to 0.1.
#' @param maxiter the maximum number of iterations to run before the NSGA search
#' is halted.
#' @param run the number of consecutive generations without any improvement in
#' the best fitness value before the NSGA is stopped.
#' @param maxFitness the upper bound on the fitness function after that the NSGA
#' search is interrupted.
#' @param names a vector of character strings providing the names of decision
#' variables.
#' @param suggestions a matrix of solutions strings to be included in the
#' initial population. If provided the number of columns must match the number
#' of decision variables.
#' @param monitor a logical or an R function which takes as input the current
#' state of the nsga-class object and show the evolution of the search. By
#' default, for interactive sessions the function nsgaMonitor prints the average
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
#' @references N. Srinivas and K. Deb, "Multiobjective Optimization Using
#' Nondominated Sorting in Genetic Algorithms, in Evolutionary Computation,
#' vol. 2, no. 3, pp. 221-248, Sept. 1994, doi: 10.1162/evco.1994.2.3.221.
#'
#' Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation,
#' parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' @seealso [nsga2()], [nsga3()]
#'
#' @return Returns an object of class nsga1-class. See [nsga1-class] for a
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
#' #Not run:
#' \dontrun{
#' result <- nsga(type = "real-valued",
#'                fitness = zdt1,
#'                lower = c(0,0),
#'                upper = c(1,1),
#'                popSize = 100,
#'                dshare = 1,
#'                monitor = FALSE,
#'                maxiter = 500)
#' }
#'
#' @export
nsga <- function (type = c("binary", "real-valued", "permutation"),
    fitness, ...,
    lower, upper, nBits,
    population = nsgaControl(type)$population,
    selection = nsgaControl(type)$selection,
    crossover = nsgaControl(type)$crossover,
    mutation = nsgaControl(type)$mutation,
    popSize = 50,
    nObj = ncol(fitness(matrix(10000, ncol = 100, nrow = 100))),
    dshare,
    pcrossover = 0.8,
    pmutation = 0.1,
    maxiter = 100,
    run = maxiter,
    maxFitness = Inf,
    names = NULL,
    suggestions = NULL,
    monitor = if (interactive()) nsgaMonitor else FALSE,
    summary = FALSE,
    seed = NULL)
{
    call <- match.call()

    type <- match.arg(type, choices = eval(formals(nsga2)$type))

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

    dum_Fitness <- matrix(NA, nrow = popSize, ncol = nObj);
    initialDummy <- popSize
    delta_dum <- 0.1 * initialDummy

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
        #if ((length(lower) != nObj) & (length(upper) != nObj))
        #  stop("The lower and upper limits must be vector of the same number of objectives")
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
        stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
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

    #Creacion del objetivo tipo nsga
    object <- new("nsga1",
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
        dumFitness = dum_Fitness,
        dShare = dshare,
        deltaDummy = delta_dum,
        fitness = Fitness,
        summary = fitnessSummary)

    #Generate initial population
    if (maxiter == 0)
      return(object)

    switch(type,
      binary = {
        Pop <- matrix(as.double(NA), nrow = popSize, ncol = nBits)
      },
      `real-valued` = {
        Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
      },
      permutation = {
        Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
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

    object@population <- Pop
    object@fitness <- Fitness

    #First Non-dominated Ranking
    out <- non_dominated_fronts(object)
    object@f <- out$fit
    object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
    object@dumFitness <- sharing(object)

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
      object@population <- Pop
      object@fitness <- Fitness

      #Evaluate Fitness
      for (i in seq_len(popSize)) {
        if (is.na(Fitness[i])) {
          fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
          Fitness[i,] <- fit
        }
      }

      object@population <- Pop
      object@fitness <- Fitness

      out <- non_dominated_fronts(object)
      object@f <- out$fit
      object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
      object@dumFitness <- sharing(object)
      rm(out)

      if (summary == TRUE) {
        fitnessSummary[[iter]] <- progress(object, callArgs)
        object@summary <- fitnessSummary
      } else {
        object@summary <- list(NULL)
      }

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

# @export
#' @rdname plot-methods
#' @aliases plot,nsga1-method
setMethod("plot", signature(x="nsga1", y="missing"), .get.plotting)

# @export
#' @rdname progress-methods
#' @aliases progress,nsga1-method
setMethod("progress", "nsga1", .nsga1.progress)

# @export
#' @rdname getDummyFitness-methods
#' @aliases getDummyFitness,nsga1-method
setMethod("getDummyFitness", "nsga1",
          function(obj) {
            cat("NSGA-I Dummy Fitness: \n")
            cat("\n#========================================#\n")
            print(obj@dumFitness)
            n_dum <- ncol(obj@dumFitness)
            dum_Fitness <- data.frame(obj@dumFitness)
            colnames(dum_Fitness) <- sprintf("FitDummy_%s",seq(n_dum))
            return(invisible(dum_Fitness))
          }
)

# @export
#' @rdname print-methods
#' @aliases print,nsga1-method
setMethod("print", "nsga1",
          function(x, ...) {
            algorithm <- class(x)[1]
            # Print
            cat("\nSlots Configuration:\n")
            print((slotNames(x)))
            cat("\n#========================================#\n")
            cat("\nTotal iterations: ", x@iter)
            cat("\nRepresentation Type: ", x@type)
            cat("\nPopulation size: ", x@popSize)
            if (x@type == "binary") {
              cat("\nNumber of Bits: ", x@nBits)
            } else{
              cat("\nLower Bounds: ", x@lower)
              cat("\nLower Bounds: ", x@upper)
            }
            cat("\nDelta Distance (dShare):  ", x@dShare)
            cat("\nDistance of sharing function: ", x@deltaDummy)
            cat("\nNumber of Nondominated Front: ", length(x@f[[1]]))
            cat("\n#========================================#\n")

          }
)


# @export
#' @rdname summary-methods
#' @aliases summary,nsga1-method
setMethod("summary", "nsga1",
          function(object, ...){
            callArgs <- list(...)
            nullRP <- is.null(callArgs$reference_dirs)

            # Calculate information for summary

            first <- object@f[[1]]
            first_front_fit <-
              first_front_pop <- object@population[first, ]
            nadir_point <- apply(object@fitness[first, ], 2, max)

            #first_dum <- object@dumFitness[first, ] for nsga1 summary method

            if("ecr" %in% rownames(utils::installed.packages())){
              if (nullRP) {
                cat("Warning! \nReference points not provided:\n
                      value necessary to evaluate GD and IGD.")

              } else{
                gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
                igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
              }
            }

            if("emoa" %in% rownames(utils::installed.packages())){
              if(nullRP) {
                cat("\nUsing the maximum in each dimension to evaluate Hypervolumen")
                reference_point <- nadir_point
              } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
              hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
            }

            cat("\nSummary of NSGA-I run")
            cat("\n#====================================")
            cat("\nNumber of Objectives evaluated: ", ncol(object@fitness))
            cat("\nTotal iterations: ", object@iter)
            cat("\nPopulation size: ", object@popSize)
            #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
            cat("\nNondominated points found: ", length(first),
                paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
                "of total)")
            cat("\nShare Distance: ", object@dShare)
            cat("\nSharing Values calculated: ", object@deltaDummy)
            #cat("\nEstimated nadir point: ", round(object@nadir_point, 3))
            cat("\nMutation Probability: ",
                paste0(signif(100 * object@pmutation, 3), "%"))
            cat("\nCrossover Probability: ",
                paste0(signif(100 * object@pcrossover, 3), "%"))
            if("ecr" %in% rownames(utils::installed.packages())){
              if(!nullRP) cat("\nEstimated IGD: ", igd)
              if(!nullRP) cat("\nEstimated GD: ", gd)
            } else cat("\n\nPlease install package 'ecr' to calculate IGD and GD.")
            if("emoa" %in% rownames(utils::installed.packages())) {
              cat("\nEstimated HV: ", hv)
              cat("\nRef point used for HV: ", reference_point)
            } else cat("\n\nPlease install package 'emoa' to calculate hypervolume.")
            cat("\n#====================================")
          }
)
