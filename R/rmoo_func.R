#' R Multi-Objective Optimization
#'
#' Minimization of a fitness function using Non-Dominated Genetic algorithms
#' (NSGA). Local search using general-purpose optimisation algorithms can be
#' applied stochastically to exploit interesting regions.
#'
#' Minimization of a fitness function using non-dominated sorting genetic
#' algorithms - II (NSGA-IIs). Multiobjective evolutionary algorithms
#'
#' The Non-dominated genetic algorithms is a meta-heuristic proposed by
#' N. Srinivas and K. Deb in 1994. The purpose of the algorithms is to find an
#' efficient way to optimize multi-objectives functions (two or more).
#'
#' The Non-dominated genetic algorithms II is a meta-heuristic proposed by
#' K. Deb, A. Pratap, S. Agarwal and T. Meyarivan in 2002. The purpose of the
#' algorithms is to find an efficient way to optimize multi-objectives functions
#' (two or more).
#'
#' Minimization of a fitness function using non-dominated sorting genetic
#' algorithms - III (NSGA-IIIs). Multiobjective evolutionary algorithms
#'
#' The Non-dominated genetic algorithms III is a meta-heuristic proposed by
#' K. Deb and H. Jain in 2013.
#' The purpose of the algorithms is to find an efficient way to optimize
#' multi-objectives functions (more than three).
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
#' @param ... additional arguments to be passed to the fitness function. This
#' allows to write fitness functions that keep some variables fixed during the
#' search.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references K. Deb, A. Pratap, S. Agarwal and T. Meyarivan, 'A fast and
#' elitist multiobjective genetic algorithm: NSGA-II,' in IEEE Transactions on
#' Evolutionary Computation, vol. 6, no. 2, pp. 182-197, April 2002,
#' doi: 10.1109/4235.996017.
#'
#' Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation,
#' parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' @references K. Deb and H. Jain, "An Evolutionary Many-Objective Optimization
#' Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I:
#' Solving Problems With Box Constraints," in IEEE Transactions on Evolutionary
#' Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014,
#' doi: 10.1109/TEVC.2013.2281535.
#'
#' Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation,
#' parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' @return Returns an object of class nsga2-class. See [nsga2-class] for a
#' description of available slots information.
#'
#' @return Returns an object of class nsga3-class. See [nsga3-class] for a
#' description of available slots information.
#'
#' @return Returns an object of class nsga-class. See [nsga-class] for a
#' description of available slots information.
#'
#' @references N. Srinivas and K. Deb, "Multiobjective Optimization Using
#' Nondominated Sorting in Genetic Algorithms, in Evolutionary Computation,
#' vol. 2, no. 3, pp. 221-248, Sept. 1994, doi: 10.1162/evco.1994.2.3.221.
#'
#' Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation,
#' parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' @seealso [nsga()], [nsga2()], [nsga3()]
#'
#' @examples
#' #Example 1
#' #Two Objectives - Real Valued
#' zdt1 <- function (x, ...) {
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
#' result <- rmoo(type = "real-valued",
#'                fitness = zdt1,
#'                lower = c(0,0),
#'                upper = c(1,1),
#'                popSize = 100,
#'                dshare = 1,
#'                monitor = FALSE,
#'                maxiter = 500,
#'                strategy = "NSGA-I")
#' }
#'
#' #Example 2
#' #Three Objectives - Real Valued
#' dtlz1 <- function (x, nobj = 3, ...){
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
#' #Not run:
#' \dontrun{
#' result <- rmoo(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = c(0,0,0),
#'                 upper = c(1,1,1),
#'                 popSize = 92,
#'                 monitor = FALSE,
#'                 maxiter = 500,
#'                 strategy = "NSGA-II")
#' }
#' @examples
#' #Example 3
#' #Two Objectives - Real Valued
#' zdt1 <- function (x, ...) {
#'  if (is.null(dim(x))) {
#'    x <- matrix(x, nrow = 1)
#'  }
#'  n <- ncol(x)
#'  g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#'  return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
#' }
#'
#' #Not run
#' \dontrun{
#' result <- rmoo(type = "real-valued",
#'                 fitness = zdt1,
#'                 lower = c(0,0),
#'                 upper = c(1,1),
#'                 popSize = 100,
#'                 n_partitions = 100,
#'                 monitor = FALSE,
#'                 maxiter = 500,
#'                 strategy = "NSGA-III")
#' }
#'
#' #Example 4
#' #Three Objectives - Real Valued
#' dtlz1 <- function (x, nobj = 3, ...){
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
#' #Not Run
#' \dontrun{
#' result <- rmoo(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = c(0,0,0),
#'                 upper = c(1,1,1),
#'                 popSize = 92,
#'                 n_partitions = 12,
#'                 monitor = FALSE,
#'                 maxiter = 500,
#'                 strategy = "NSGA-III")
#' }
#'
#' @export
rmoo <- function (...) {

  call <- match.call()

  callArgs <- list(...)

  rmoo.input.pars <- as.list(sys.call())[-1]

  algorithm <- match.arg(callArgs$strategy, choices = c('GA', 'NSGA-I', 'NSGA-II', 'NSGA-III'))

  if (!is.null(callArgs$summary)){
    if(is.null(callArgs$reference_dirs) && (callArgs$summary == TRUE)){
      if(algorithm != "GA") {
          cat("Warning: reference points not provided:\n
              HV, GD and IGD will not be evaluated during execution.")
      }
    }
  }
  out <- getAlgorithm(rmoo.input.pars, algorithm)
  out@call <- match.call()
  return(out)
}

getAlgorithm <- function(rmoo.input.pars, algorithm, ...){
  if (algorithm == "NSGA-I") {
    do.call(rmoo::nsga,
            args = rmoo.input.pars)
  } else if (algorithm == "NSGA-II") {
    do.call(rmoo::nsga2,
            args = rmoo.input.pars)
  } else if (algorithm == "NSGA-III"){
    do.call(rmoo::nsga3,
            args = rmoo.input.pars)
  } else if (algorithm == "GA"){
    do.call(GA::ga,
            args = rmoo.input.pars)
  }
}

dtlz1 <- function (x, nobj = 3, ...){
  if (is.null(dim(x))) {
    x <- matrix(x, 1)
  }
  n <- ncol(x)
  y <- matrix(x[, 1:(nobj - 1)], nrow(x))
  z <- matrix(x[, nobj:n], nrow(x))
  g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 * pi * (z - 0.5))))
  tmp <- t(apply(y, 1, cumprod))
  tmp <- cbind(t(apply(tmp, 1, rev)), 1)
  tmp2 <- cbind(1, t(apply(1 - y, 1, rev)))
  f <- tmp * tmp2 * 0.5 * (1 + g)
  return(f)
}

nsga3 <- rmoo(type = "real-valued",
     fitness = dtlz1,
     lower = c(0,0,0),
     upper = c(1,1,1),
     monitor = FALSE,
     popSize = 92,
     reference_dirs = rmoo::generate_reference_points,
     n_partitions = 12, summary = TRUE,
     maxiter = 5,
     strategy = "NSGA-III")

nsga2 <- rmoo(type = "real-valued",
     fitness = dtlz1,
     lower = c(0,0,0),
     upper = c(1,1,1),
     monitor = FALSE,
     popSize = 92,summary = TRUE,
     maxiter = 5,
     strategy = "NSGA-II")

nsga1 <- rmoo(type = "real-valued",
     fitness = dtlz1,
     lower = c(0,0,0),
     upper = c(1,1,1),
     monitor = FALSE,
     dshare = 1,
     popSize = 92,summary = TRUE,
     maxiter = 5,
     strategy = "NSGA-I")

rmoo(type = "real-valued",
     fitness = zdt1,
     lower = c(0,0),
     upper = c(1,1),
     popSize = 100,
     monitor = FALSE,
     maxiter = 1,
     strategy = "NSGA-II")

#Test for GA2
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

ga <- rmoo(type = "real-valued",
     fitness = function(x, ...) -Rastrigin(x[1], x[2]),
     lower = c(-5.12, -5.12), upper = c(5.12, 5.12),
     popSize = 50, maxiter = 100,
     strategy = "GA")

#Test for GA
f <- function(x,  ...)  (x^2+x)*cos(x)
ga <- rmoo(type = "real-valued",
           fitness = f,
           lower = -20, upper = 20,
           maxiter = 1, strategy = "GA")


#colMax <- function(data) apply(data, 2, max)

summary.rmoo <- function(object, ...) {
  callArgs <- list(...)
  algorithm <- class(object)[1]
  if(algorithm == "nsga1"){
    out <- nsgaSummary(object, callArgs)
  }
  else if(algorithm == "nsga2"){
    out <- nsgaiiSummary(object, callArgs)
  }
  else if(algorithm == "nsga3"){
    out <- nsgaiiiSummary(object, callArgs)
  }
  return(out)
}

#Agregar estas funciones en las funciones principales de NSGAs
nsgaSummary <- function(object, ...) {
  # Calculate information for summary
  callArgs <- list(...)

  nullRP <- is.null(callArgs$reference_dirs)

  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  first_dum <- object@dumFitness[first, ]

  if("ecr" %in% rownames(utils::installed.packages())){
    if (!nullRP) {
      gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
      igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
    }
  }

  if("emoa" %in% rownames(utils::installed.packages())){
    if(!nullRP) {
      reference_point <- apply(callArgs$reference_dirs, 2, max)
      hv <- emoa::dominated_hypervolume(points = t(first_front_fit), ref = reference_point)
    }
  }

  if(nullRP) {
    result <- list(`First Front Fit` = first_front_fit,
                   `First Front Pop` = first_front_pop,
                   `Dummy Front Fit` = first_dum)

  } else{
    result <- list(`First Front Fit` = first_front_fit,
                   `First Front Pop` = first_front_pop,
                   `Dummy Front Fit` = first_dum,
                   Hypervolumen = hv,
                   `Generational Distance` = gd,
                   `InvertedGenerational Distance` = igd)

  }
  return(result)
}

#Summary for NSGA-II Algorithm
nsgaiiSummary <- function(object, ...) {
  # Calculate information for summary
  callArgs <- list(...)

  nullRP <- is.null(callArgs$reference_dirs)

  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  first_cd <- object@crowdingDistance[first, ]

  if("ecr" %in% rownames(utils::installed.packages())){
    if (!nullRP) {
      gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
      igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
    }
  }
  if("emoa" %in% rownames(utils::installed.packages())){
    if(!nullRP) {
      reference_point <- apply(callArgs$reference_dirs, 2, max)
      hv <- emoa::dominated_hypervolume(points = t(first_front_fit), ref = reference_point)
    }
  }

  if(nullRP) {
    result <- list(`First Front Fit` = first_front_fit,
                   `First Front Pop` = first_front_pop,
                   `Crowding Dist` = first_cd)

  } else{
    result <- list(`First Front Fit` = first_front_fit,
                   `First Front Pop` = first_front_pop,
                   `Crowding Dist` = first_cd,
                   Hypervolumen = hv,
                   `Generational Distance` = gd,
                   `InvertedGenerational Distance` = igd)

  }

  return(result)
}

#Summary for NSGA-III Algorithm
nsgaiiiSummary <- function(object, ...) {
  # Calculate information for summary
  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  ideal_point <- object@ideal_point
  worst_point <- object@worst_point
  extreme_points <- object@extreme_points

  if("ecr" %in% rownames(utils::installed.packages())){
    gd <- ecr::computeGenerationalDistance(t(object@fitness), t(object@reference_points))
    igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(object@reference_points))
  }
  if("emoa" %in% rownames(utils::installed.packages())){
    hv <- ecr::computeHV(t(object@fitness), ref.point = apply(object@reference_points, 2, max))
  }
  if (all((c("ecr", "emoa") %in% rownames(utils::installed.packages())))) {
    metric <- data.frame(Iternation = object@iter,
                         Generational_Distance = gd,
                         Inverse_Generational_Distance = igd,
                         Hypervolumen = hv)
    result <- list(first_front_fit = first_front_fit,
                   first_front_pop = first_front_pop,
                   ideal_point = ideal_point,
                   worst_point = worst_point,
                   extreme_points = extreme_points,
                   metrics = metric)
  } else{
    result <- list(first_front_fit = first_front_fit,
                   first_front_pop = first_front_pop,
                   ideal_point = ideal_point,
                   worst_point = worst_point,
                   extreme_points = extreme_points)
  }
  return(result)
}






#------------------------------------------------------------------------------
#import("stats", "graphics", "methods", "foreach", "iterators") ADD GA TO import in NAMESPACE and depends in description
#importFrom("grDevices", "adjustcolor", "colorRampPalette")


nsgaAlgorithm <- function(...) {
  current <- .nsga.algorithm
  if (nargs() == 0)
    return(current)
  args <- list(...)
  if (length(args) == 1 && is.null(names(args))) {
    arg <- args[[1]]
    switch(mode(arg),
           list = args <- arg,
           character = return(.nsga.algorithm[[arg]]),
           stop("invalid argument: ", dQuote(arg))
    )
  }

  if (length(args) == 0)
    return(current)
  nargs <- names(args)
  if (is.null(nargs))
    stop("options must be given by name")

  if (is.list(args)) {
    changed <- current[nargs]
    for (i in 1:length(nargs)) {
      if (is.list(args[[i]])) {
        what <- names(args[[i]])
        changed[[i]][what] <- args[[i]][what]
      } else {
        changed[i] <- args[[i]]
      }
    }
    current[nargs] <- changed
  } else {
    changed <- current[nargs]
    current[nargs] <- args
  }

  if (sys.parent() == 0)
    env <- asNamespace("rmoo") else env <- parent.frame()
  assign(".nsga.algorithm", current, envir = env)
  invisible(current)
}

.nsga.algorithm <- list(GA = "ga",
                        `NSGA-I` = "nsga",
                        `NSGA-II` = "nsga2",
                        `NSGA-III` = "nsga3")


rmoo.func <- function (...) {

  #strategy = c("GA", "NSGA-I", "NSGA-II", "NSGA-III"),
  #algorithm = nsgaAlgorithm(strategy),

  call <- match.call()

  callArgs <- list(...)

  rmoo.input.pars <- as.list(sys.call())[-1]

  strategy <- match.arg(callArgs$strategy, choices = c('GA', 'NSGA-I', 'NSGA-II', 'NSGA-III'))
  #strategy <- match.arg(rmoo.input.pars$strategy, choices = eval(formals(rmoo.func)$strategy))

  #Agregar al metodo de summary
  if(is.null(callArgs$reference_dirs) && (callArgs$summary == TRUE)){
    if(algorithm != "GA") {
      cat("Warning: reference point not provided:\n
          using the maximum in each dimension instead.")
      #rmoo.input.pars$reference_dirs <- TRUE

    }
  }

  if (!is.function(algorithm))
    algorithm <- get(algorithm)

  out <- algorithm(rmoo.input.pars)
  out@call <- match.call()
  return(out)
}

#------------------------------------------------------------------------------

rmoo <- function (type = c("binary", "real-valued", "permutation"),
                  strategy = c("GA","NSGA-I", "NSGA-II", "NSGA-III"),
                   fitness, ...,
                   lower, upper, nBits,
                   population = nsgaControl(type)$population,
                   selection = nsgaControl(type)$selection,
                   crossover = nsgaControl(type)$crossover,
                   mutation = nsgaControl(type)$mutation,
                   popSize = 50,
                   nObj = ncol(fitness(matrix(10000, ncol = 100, nrow = 100))),
                   dshare = NULL,
                   pcrossover = 0.8,
                   pmutation = 0.1,
                   stopcrit = NULL,
                   maxiter = 100,
                   run = maxiter,
                   maxFitness = Inf,
                   names = NULL,
                   suggestions = NULL,
                   monitor = if (interactive()) nsgaMonitor else FALSE,
                   summary = FALSE,
                   n_partitions = NULL,
                   reference_dirs = generate_reference_points,
                   seed = NULL)
{
  rmoo.input.pars <- as.list(environment())

  rmoo.input.pars$call <- match.call()

  rmoo.input.pars$strategy <- match.arg(type, choices = eval(formals(rmoof)$strategy))

  rmoo.input.pars$type <- match.arg(type, choices = eval(formals(nsga3)$type))

  callArgs <- list(...)

  if (!is.null(strategy)) {
    rmoo.input.pars$algorithm <- strategy$name

  }

  if (is.null(algorithm))
    algorithm <- "NSGA-II"

  if (!is.function(population))
    population <- get(population)
  if (!is.function(selection))
    selection <- get(selection)
  if (!is.function(crossover))
    crossover <- get(crossover)
  if (!is.function(mutation))
    mutation <- get(mutation)



  if (is.function(reference_dirs) & is.null(popSize)) {
    popSize <- nrow(reference_dirs(nObj, n_partitions))
  } else {
    if (is.matrix(reference_dirs) & is.null(popSize)) {
      popSize <- nrow(reference_dirs)
    }
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


  if (!is.function(reference_dirs) & !is.matrix(reference_dirs)) {
    stop("A Determination of Reference Points function
            or matrix must be provided.")
  }

  if (is.null(nObj)) {
    nObj <- ncol(fitness(matrix(10000, ncol = 100, nrow = 100)))
  }

  #For NSGA-III
  #Generate reference points, otherwise, assign the provided matrix
  if (is.function(reference_dirs)) {
    ref_dirs <- reference_dirs(nObj, n_partitions)
  } else {
    ref_dirs <- reference_dirs
  }

  if (ncol(ref_dirs) != nObj) {
    stop("Dimensionality of reference points must be equal to the number of objectives")
  }

  #For NSGA-I
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

  object <- object.definition(args = as.list(environment()))

  if (maxiter == 0)
    return(object)
  #Generate initial population--------------------------------------------

  #p_fit <- q_fit <- matrix(as.double(NA), nrow = popSize, ncol = nObj)
  switch(type,
          binary = {
            Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nBits)
          },
          `real-valued` = {
            Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
          },
          permutation = {
            Pop <- P <- Q <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
          }
  )

  #----------------------------------------

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
  object@dumFitness <- sharing(object) #NSGA-I
  object@crowdingDistance <- matrix(as.double(NA), nrow = popSize) #NSGA-II
  n_remaining <- popSize #NSGA-III

  keep.running <- TRUE

  while(keep.running){
    # Update iteration counter

    object@iter <- object@iter + 1

    elapsed.time <- as.numeric(difftime(time1 = Sys.time(),
                                        time2 = time.start,
                                        units = "secs"))
    iter.times[iter] <- ifelse(iter == 1,
                               yes = as.numeric(elapsed.time),
                               no  = as.numeric(elapsed.time) - sum(iter.times))


    if (summary == TRUE) {
      fitnessSummary[[iter]] <- summary(object)
      object@summary <- fitnessSummary
    } else {
      object@summary <- list()
    }

    #Plot front non-dominated by iteration
    if (is.function(monitor)) {
      monitor(object = object, number_objective = nObj)
    }

    if (max(Fitness, na.rm = TRUE) >= maxFitness)
      break
    if (object@iter == maxiter)
      break

    # Verify stop criteria
    keep.running <- check_stop_criteria(stopcrit = stopcrit,
                                        call.env = environment())

  }

}

Xv <- do.call(perform_variation, args = as.list(environment()))

object@population <- Pop <- Xv$Pop
object@fitness <- Fitness <- Xv$Fit

perform_variation <- function(operators, ...){
      object <- operators$object
      popSize <- operators$popSize
      selection <- get(operators$selection)
      #Selection Operator
      if (is.function(selection)) {
        sel <- selection(object, operators$nObj)
        Pop <- sel$population
        Fitness <- sel$fitness
      } else {
        sel <- sample(1:popSize, size = popSize, replace = TRUE)
        Pop <- object@population[sel, ]
        Fitness <- object@fitness[sel, ]
      }
      object@population <- Pop
      object@fitness <- Fitness

      crossover <- get(operators$crossover)

      #Cross Operator
      if (is.function(crossover) & operators$pcrossover > 0) {
        nmating <- floor(popSize / 2)
        mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)
        for (i in seq_len(nmating)) {
          if (operators$pcrossover > runif(1)) {
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
      pm <- if (is.function(operators$pmutation)) {
        pmutation(object)
      } else {pmutation}

      mutation <- get(operators$mutation)
      if (is.function(mutation) & pm > 0) {
        for (i in seq_len(popSize)) {
          if (pm > runif(1)) {
            Mutation <- mutation(object, i)
            Pop[i, ] <- Mutation
            Fitness[i,] <- NA
          }
        }
      }

      return(list(Pop = Pop,
                  Fit = Fitness))

}


# Evaluate Fitness
for (i in seq_len(popSize)) {
  if (is.na(Fitness[i])) {
    fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
    Fitness[i, ] <- fit
  }
}

object@population <- Q <- Pop
object@fitness <- q_fit <- Fitness

update_population <- function(){
  #NSGA-I-------------------------------------------
  out <- non_dominated_fronts(object)
  object@f <- out$fit
  object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)
  object@dumFitness <- sharing(object)
  rm(out)
  #-------------------------------------------------

  #NSGA-II------------------------------------------
  # R = P U Q
  object@population <- Pop <- rbind(P, Q)
  object@fitness <- rbind(p_fit, q_fit)

  out <- non_dominated_fronts(object)
  object@f <- out$fit
  object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

  cd <- crowding_distance(object, nObj)
  object@crowdingDistance <- cd

  # Sorted porpulation and fitness by front and crowding distance
  populationsorted <- object@population[order(object@front, -object@crowdingDistance), ]
  fitnesssorted <- object@fitness[order(object@front, -object@crowdingDistance), ]

  # Select de first N element
  object@population <- P <- Pop <- populationsorted[1:popSize, ]
  object@fitness <- p_fit <- fitnesssorted[1:popSize, ]

  out <- non_dominated_fronts(object)
  object@f <- out$fit
  object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

  cd <- crowding_distance(object, nObj)
  object@crowdingDistance <- cd
  #-------------------------------------------------

  #NSGA-III-----------------------------------------
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

  # worst_of_front <- apply(object@fitness[object@f[[1]], ], 2, max)
  # If the first front is by a single fit
  worst_of_front <- if (length(object@f[[1]]) == 1)
    object@fitness[object@f[[1]], ]
  else apply(object@fitness[object@f[[1]], ], 2, max)

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

  # outniches <- associate_to_niches(object)
  # If the first front is by a single fit
  outniches <- if (length(object@f[[1]]) == 1)
    associate_to_niches(object, utopian_epsilon = 0.00001)
  else associate_to_niches(object)
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
  #-------------------------------------------------


}



#-------------------------------------------------
problem   <- list(name       = "problem.sr",
                  xmin       = rep(-1, 30),
                  xmax       = rep(1, 30),
                  m          = 2)
decomp    <- list(name       = "SLD", H = 49) # <-- H = 99 in the original
neighbors <- list(name       = "lambda",
                  T          = 20,
                  delta.p    = 1)
aggfun    <- list(name       = "wt")
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 1),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.1),
                  list(name  = "truncate"))
update    <- list(name       = "standard", UseArchive = FALSE)
scaling   <- list(name       = "none")
constraint<- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                    maxiter  = 50))      # <-- maxiter = 200 in the original
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- 42


out1 <- moead(preset = preset_moead("original"),
              problem,
              decomp,
              aggfun,
              neighbors,
              variation,
              update,
              constraint,
              scaling,
              stopcrit,
              showpars,
              seed)


#https://statisticsglobe.com/error-unused-argument-in-r

rmoo <- function (type = c("binary", "real-valued", "permutation"),
                  strategy = c("NSGA-I", "NSGA-II", "NSGA-III"),
                  fitness, ...,
                  lower, upper, nBits,
                  population = nsgaControl(type)$population,
                  selection = nsgaControl(type)$selection,
                  crossover = nsgaControl(type)$crossover,
                  mutation = nsgaControl(type)$mutation,
                  popSize = 50,
                  nObj = ncol(fitness(matrix(10000, ncol = 100, nrow = 100))),
                  dshare = NULL,
                  pcrossover = 0.8,
                  pmutation = 0.1,
                  stopcrit = NULL,
                  maxiter = 100,
                  run = maxiter,
                  maxFitness = Inf,
                  names = NULL,
                  suggestions = NULL,
                  monitor = if (interactive()) nsgaMonitor else FALSE,
                  summary = FALSE,
                  n_partitions = NULL,
                  reference_dirs = NULL,
                  seed = NULL)
{

  # rmoo.input.pars <- as.list(sys.call())[-1]
  #
  # match.call save the used argument in the function
  # rmoo.input.pars$call <- match.call()
  #
  # rmoo.input.pars$strategy <- match.arg(strategy, choices = eval(formals(rmoo)$strategy))
  #
  # rmoo.input.pars$type <- match.arg(type, choices = eval(formals(rmoo)$type))
  #
  # rmoo.input.pars$callArgs <- list(...)
  #
  # rmoo.input.pars <- as.list(environment())

  # match.call save the used argument in the function
  call <- match.call()

  callArgs <- list(...)

  rmoo.input.pars <- as.list(sys.call())[-1]

  algorithm <- match.arg(callArgs$strategy, choices = c('GA', 'NSGA-I', 'NSGA-II', 'NSGA-III'))
  #strategy <- match.arg(strategy, choices = eval(formals(rmoo)$strategy))

  #type <- match.arg(type, choices = eval(formals(rmoo)$type))

  #rmoo.input.pars <- as.list(environment())

  #rmoo.input.pars$algorithm <- rmoo.input.pars$strategy

  if(is.null(callArgs$reference_dirs)){
    if(algorithm != "GA") {
      cat("Warning: reference point not provided:\n
          using the maximum in each dimension instead.")
      rmoo.input.pars$reference_dirs <- TRUE

    }
  }
  getAlgorithm(rmoo.input.pars, algorithm)

  # if (!is.null(rmoo.input.pars$strategy)) {
  #   rmoo.input.pars$algorithm <- rmoo.input.pars$strategy
  #   getAlgorithm(rmoo.input.pars)
  # } else {
  #   stop(" ")
  # }

}



getAlgorithm <- function(rmoo.input.pars, algorithm, ...){
  if (algorithm == "NSGA-I") {
    do.call(rmoo::nsga,
            args = rmoo.input.pars)
  } else if (algorithm == "NSGA-II") {
    do.call(rmoo::nsga2,
            args = rmoo.input.pars)
  } else if (algorithm == "NSGA-III"){
    do.call(rmoo::nsga3,
            args = rmoo.input.pars)
  } else if (algorithm == "GA"){
    do.call(GA::ga,
            args = rmoo.input.pars)
  }
}


# getAlgorithm <- function(rmoo.input.pars, ...){
#   if (rmoo.input.pars$algorithm == "NSGA-I") {
#     do.call(rmoo::nsga,
#             args = rmoo.input.pars)
#   } else if (rmoo.input.pars$algorithm == "NSGA-II") {
#     do.call(rmoo::nsga2,
#             args = rmoo.input.pars)
#   } else if (rmoo.input.pars$algorithm == "NSGA-III"){
#     do.call(rmoo::nsga3,
#             args = rmoo.input.pars)
#   } else if (rmoo.input.pars$algorithm == "GA"){
#     do.call(GA::ga,
#             args = rmoo.input.pars)
#   }
# }
#ref_point

