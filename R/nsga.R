#' Non-Dominated Sorting in Genetic Algorithms
#'
#' Minimization of a fitness function using genetic algorithms (GAs).
#' Local search using general-purpose optimisation algorithms can be applied stochastically to exploit interesting regions.
#'
#' The Non-dominated genetic algorithms is a meta-heuristic proposed by N. Srinivas and K. Deb in 1994.
#' The purpose of the algorithms is to find an efficient way to optimize multi-objectives functions (two or more).
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
#' @param dshare the maximun phenotypic distance allowed between any two individuals to become members of a niche.
#' @param pcrossover the probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8.
#' @param pmutation the probability of mutation in a parent chromosome. Usually mutation occurs with a small probability, and by default is set to 0.1.
#' @param elitism the number of best fitness individuals to survive at each generation. By default the top 5% individuals will survive at each iteration.
#' @param updatePop a logical defaulting to FALSE. If set at TRUE the first attribute attached to the value returned by the user-defined fitness function is used to update the population.
#' Be careful though, this is an experimental feature!
#' @param postFitness a user-defined function which, if provided, receives the current nsga-class object as input, performs post fitness-evaluation steps, then returns an updated version of the object which is used to update the NSGA search.
#' Be careful though, this is an experimental feature!
#' @param maxiter the maximum number of iterations to run before the NSGA search is halted.
#' @param run the number of consecutive generations without any improvement in the best fitness value before the NSGA is stopped
#' @param maxFitness the upper bound on the fitness function after that the NSGA search is interrupted.
#' @param names a vector of character strings providing the names of decision variables.
#' @param suggestions a matrix of solutions strings to be included in the initial population. If provided the number of columns must match the number of decision variables.
#' @param optim a logical defaulting to FALSE determining whether or not a local search using general-purpose optimisation algorithms should be used. See argument optimArgs for further details and finer control.
#' @param optimArgs a list controlling the local search algorithm with the following components:
#' @param keepBest a logical argument specifying if best solutions at each iteration should be saved in a slot called bestSol. See [nsga-class].
#' @param parallel An optional argument which allows to specify if the Genetic Algorithm should be run sequentially or in parallel.
#' @param monitor a logical or an R function which takes as input the current state of the nsga-class object and show the evolution of the search. By default, for interactive sessions the function nsgaMonitor prints the average and best fitness values at each iteration. If set to plot these information are plotted on a graphical device. Other functions can be written by the user and supplied as argument. In non interactive sessions, by default monitor = FALSE so any output is suppressed.
#' @param seed an integer value containing the random number generator state. This argument can be used to replicate the results of a NSGA search. Note that if parallel computing is required, the doRNG package must be installed.
#'
#' @author Francisco Benitez
#' \email{benitez.fj@@hotmail.com}
#'
#' @references N. Srinivas and K. Deb, "Multiobjective Optimization Using Nondominated Sorting in Genetic Algorithms, in Evolutionary Computation, vol. 2, no. 3, pp. 221-248, Sept. 1994, doi: 10.1162/evco.1994.2.3.221.
#'
#' Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
#' \url{http://www.jstatsoft.org/v53/i04/}.
#'
#' @seealso [nsga2()], [nsga3()]
#'
#' @return Returns an object of class nsga-class. See [nsga-class] for a description of available slots information.
nsga <- function (type = c("binary", "real-valued", "permutation"),
                  fitness, ...,
                  lower, upper, nBits,
                  population = generate_population_real,
                  selection = nsga_tourSelection_R,
                  crossover = nsgareal_laCrossover_R,
                  mutation = nsgareal_raMutation_R,
                  popSize = 50,
                  nObj,
                  dshare,
                  pcrossover = 0.8,
                  pmutation = 0.1,
                  maxiter = 100,
                  run = maxiter,
                  maxFitness = Inf,
                  names = NULL,
                  suggestions = NULL,
                  optim = FALSE,
                  optimArgs = list(method = "L-BFGS-B",
                                  poptim = 0.05,
                                  pressel = 0.5,
                                  control = list(fnscale = -1, maxit = 100)),
                  keepBest = FALSE,
                  parallel = FALSE,
                  monitor = if (interactive()) gaMonitor else FALSE,
                  seed = NULL)
{
  #gaMonitor
  call <- match.call()

  type <- type

  algorithm <-"NSGA"

  nObj = ncol(fitness(matrix(10000, ncol = 100, nrow = 100)))

  #Validaciones
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
  if (elitism > popSize) {
    stop("The elitism cannot be larger that population size.")
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

  #Si el algoritmos seleccionado es distinto al NSGA el inicial Dummy y el dShare no se utilizar?n
  if(algorithm != "NSGA"){
    initialDummy <- FALSE
    dShare <- FALSE
  }else{
    DumFitness <- matrix(NA, nrow = popSize, ncol = nObj); #Solo se utilizara en el NSGA-I
    initialDummy <- popSize; #Se pasa como argumento del main
    delta_dum <- 0.1*initialDummy;
  }

  # check for min and max arguments instead of lower and upper
  callArgs <- list(...)
  if (any("min" %in% names(callArgs))) {
    lower <- callArgs$min
    callArgs$min <- NULL
    warning("'min' arg is deprecated. Use 'lower' instead.")
  }
  if (any("max" %in% names(callArgs))) {
    upper <- callArgs$max
    callArgs$max <- NULL
    warning("'max' arg is deprecated. Use 'upper' instead.")
  }

  if (missing(lower) & missing(upper) & missing(nBits)) {
    stop("A lower and upper range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!")
  }

  #Se trabajar? de forma binaria, permutacion o numeros reales y vectoriza las variables de lower y upper
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
        stop("lower and upper must be vector of the same length!")
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

  # check suggestions
  if (is.null(suggestions))
  {
    suggestions <- matrix(nrow = 0, ncol = nvars)
  }else {
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
  if (is.logical(monitor))
  { if (monitor)  monitor <- gaMonitor }
  if (is.null(monitor))  monitor <- FALSE

  # if optim merge provided and default args for optim()
  if (optim) { # merge default and provided parameters
    optimArgs.default <- eval(formals(ga)$optimArgs)
    optimArgs.default$control[names(optimArgs$control)] <- optimArgs$control
    optimArgs$control <- NULL
    optimArgs.default[names(optimArgs)] <- optimArgs
    optimArgs <- optimArgs.default
    rm(optimArgs.default)
    if (any(optimArgs$method == c("L-BFGS-B", "Brent"))) {
      optimArgs$lower <- lower
      optimArgs$upper <- upper
    }
    else {
      optimArgs$lower <- -Inf
      optimArgs$upper <- Inf
    }
    optimArgs$poptim <- min(max(0, optimArgs$poptim), 1)
    optimArgs$pressel <- min(max(0, optimArgs$pressel), 1)
    optimArgs$control$maxit <- as.integer(optimArgs$control$maxit)
    # ensure that optim maximise the fitness
    if (is.null(optimArgs$control$fnscale))
      optimArgs$control$fnscale <- -1
    if (optimArgs$control$fnscale > 0)
      optimArgs$control$fnscale <- -1 * optimArgs$control$fnscale
  }
  #Remove
  #------------------------------------------------------------
  # Start parallel computing (if needed)
  if (is.logical(parallel)) {
    if (parallel) {
      parallel <- startParallel(parallel)
      stopCluster <- TRUE
    }
    else {
      parallel <- stopCluster <- FALSE
    }
  }
  else {
    stopCluster <- if (inherits(parallel, "cluster")) {FALSE}
    else{TRUE}
    parallel <- startParallel(parallel)
  }
  on.exit(if (parallel & stopCluster)
    stopParallel(attr(parallel, "cluster")))
  # define operator to use depending on parallel being TRUE or FALSE
  `%DO%` <- if (parallel && requireNamespace("doRNG", quietly = TRUE))
    doRNG::`%dorng%`
  else if (parallel)
    `%dopar%`
  else `%do%`
  #------------------------------------------------------------

  # set seed for reproducibility
  if (!is.null(seed))
    set.seed(seed)

  i. <- NULL #dummy to trick R CMD check

  ##Inicializacion de variables
  if(is.null(nObj))  nObj <- ncol(fitness(matrix(10000, ncol = 100, nrow = 100)))

  fitnessSummary <- matrix(as.double(NA), nrow = maxiter, ncol = 6)
  colnames(fitnessSummary) <- names(gaSummary(rnorm(10)))

  bestSol <- if (keepBest){
    vector(mode = "list", length = maxiter)
  }else{
    list()
  }

  Fitness <- matrix(NA, nrow = popSize, ncol = nObj);
  Front <- vector("list", popSize);

  object <- new("nsga",
    call = call,
    type = type,
    lower = lower,
    upper = upper,
    nBits = nBits,
    names = if (is.null(names))
      character()
    else names,
    popSize = popSize,
    front = Front,
    f = list(),
    iter = 0,
    run = 1,
    maxiter = maxiter,
    suggestions = suggestions,
    population = matrix(),
    elitism = elitism,
    pcrossover = pcrossover,
    pmutation = if (is.numeric(pmutation))
      pmutation
    else NA,
    optim = optim,
    dumFitness = DumFitness,
    dShare = dshare,
    deltaDummy = delta_dum,
    fitness = Fitness,
    summary = fitnessSummary,
    bestSol = bestSol)

  if (maxiter == 0)
    return(object)
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nObj)
  ng <- min(nrow(suggestions), popSize)
  if (ng > 0) {
    Pop[1:ng, ] <- suggestions
  }
  if (popSize > ng) {
    Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - ng), ]
  }
  object@population <- Pop
  for (iter in seq_len(maxiter)) {
    object@iter <- iter

    if (!parallel) {
      for (i in seq_len(popSize)) if (is.na(Fitness[i])) {
        fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
        if (updatePop)
          Pop[i, ] <- attributes(fit)[[1]]
        Fitness[i,] <- fit
      }
    }
    else {
      Fitness <- foreach(i. = seq_len(popSize), .combine = "c") %DO%
      {
        if (is.na(Fitness[i.]))
          do.call(fitness, c(list(Pop[i., ]), callArgs))
        else Fitness[i.,]
      }
    }
    object@population <- Pop
    object@fitness <- Fitness

    object <- nondominatedfronts(object);
    object@dumFitness <- sharing(object)

    if(nObj==3){
      X <-  object@fitness
      Y <- object@f[[1]]
      Xnd <- object@fitness[Y,]
      rgl::plot3d(X)
      rgl::plot3d(Xnd, col="red", size=8, add=TRUE)
      rgl::plot3d(x=min(Xnd[,1]), y=min(Xnd[,2]), z=min(Xnd[,3]), col="green", size=8, add=TRUE)
      rgl::bgplot3d({plot.new(); title(main = iter, line = 3);});
      X.range <- diff(apply(X,2,range))
      #bounds <- rbind(apply(X,2,min)-0.1*X.range,apply(X,2,max)+0.1*X.range)
      #GPareto::plotParetoEmp(nondominatedPoints = Xnd, add=TRUE, bounds=bounds, alpha=0.5)

      #Sys.sleep(0.2)
    }else if (nObj==2) {
      X <- object@fitness
      Y <- X[object@f[[1]],]
      plot(X[,1], X[,2], col = "green", pch = 20, main= iter)
      GPareto::plotParetoEmp(cbind(Y[,1], Y[,2]), col = "red", max = TRUE)
      Sys.sleep(0.01)
    }

    fitnessSummary[iter, ] <- gaSummary(object@fitness)
    object@summary <- fitnessSummary

    if (optim & (type == "real-valued")) {
      if (optimArgs$poptim > runif(1)) {
        i <- sample(1:popSize, size = 1, prob = optimProbsel(Fitness, q = optimArgs$pressel))
        opt <- try(suppressWarnings(do.call(stats::optim,
          c(list(fn = fitness, par = Pop[i, ], method = optimArgs$method,
            lower = optimArgs$lower, upper = optimArgs$upper,
            control = optimArgs$control), callArgs))),
          silent = TRUE)
        if (is.function(monitor)) {
          if (!inherits(opt, "try-error"))
            cat("\b | Local search =", format(opt$value, digits = getOption("digits")))
          else cat("\b |", opt[1])
          cat("\n")
        }
        if (!inherits(opt, "try-error")) {
          Pop[i, ] <- opt$par
          Fitness[i] <- opt$value
        }
        object@population <- Pop
        object@fitness <- Fitness

        fitnessSummary[iter, ] <- gaSummary(object@fitness)
        object@summary <- fitnessSummary
      }
    }

    if (keepBest) {
      object@bestSol[[iter]] <- unique(Pop[Fitness == max(Fitness, na.rm = TRUE), , drop = FALSE])
    }

    if (is.function(postFitness)) {
      object <- do.call(postFitness, c(object, callArgs)) #Evaluar callArgs
      Fitness <- object@fitness
      Pop <- object@population
    }
    if (iter > 1)
      object@run <- garun(fitnessSummary[seq(iter), 1])
    if (object@run >= run)
      break
    if (max(Fitness, na.rm = TRUE) >= maxFitness)
      break
    if (object@iter == maxiter)
      break

    if (is.function(selection)) {
      sel <- selection(object, nObj)
      Pop <- sel$population
      Fitness <- sel$fitness
    }
    else {
      sel <- sample(1:popSize, size = popSize, replace = TRUE)
      Pop <- object@population[sel, ]
      Fitness <- object@fitness[sel]
    }
    object@population <- Pop
    object@fitness <- Fitness

    if (is.function(crossover) & pcrossover > 0) {
      nmating <- floor(popSize/2)
      mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)
      for (i in seq_len(nmating)) {
        if (pcrossover > runif(1)) {
          parents <- mating[i, ]
          Crossover <- crossover(object, parents)
          Pop[parents, ] <- Crossover$children
          Fitness[parents,] <- Crossover$fitness
        }
      }
      object@population <- Pop
      object@fitness <- Fitness

    }

    pm <- if (is.function(pmutation)){
      pmutation(object)
    }else{pmutation}
    if (is.function(mutation) & pm > 0) {
      for (i in seq_len(popSize)) {
        if (pm > runif(1)) {
          Mutation <- mutation(object, i)
          Pop[i, ] <- Mutation
          Fitness[i,] <- NA
        }
      }
      object@population <- Pop
      object@fitness <- Fitness

    }

    if (elitism > 0) {
      ord <- matrix(order(object@fitness, na.last = TRUE), nrow = popSize, ncol = nObj)
      u <- which(!duplicated(PopSorted, margin = 1))
      Pop[ord[1:elitism], ] <- PopSorted[u[1:elitism], ]
      Fitness[ord[1:elitism]] <- FitnessSorted[u[1:elitism]]
      object@population <- Pop
      object@fitness <- Fitness
    }
  }

  if (optim & (type == "real-valued")) {
    optimArgs$control$maxit <- rev(optimArgs$control$maxit)[1]
    i <- which.max(object@fitness)
    opt <- try(suppressWarnings(do.call(stats::optim, c(list(fn = fitness,
      par = object@population[i, ],
      method = optimArgs$method,
      lower = optimArgs$lower,
      upper = optimArgs$upper,
      control = optimArgs$control),
      callArgs))),
      silent = TRUE)
    if (is.function(monitor)) {
      if (!inherits(opt, "try-error"))
        cat("\b | Final local search =", format(opt$value,
          digits = getOption("digits")))
      else cat("\b |", opt[1])
    }
    if (!inherits(opt, "try-error")) {
      object@population[i, ] <- opt$par
      object@fitness[i] <- opt$value
    }
  }

  solution <- list(Front = object@front,
    f = object@f,
    pop = object@population,
    Fitness = object@fitness)


  return(solution)
}
