#' @include numberOrNAOrMatrix-class.R
#' Virtual Class "nsga3 - Simple Class for subassigment Values"
#'
#' The class "nsga3" is a simple class union ([setClassUnion()]) of "numeric", "logical" and "matrix".
#'
#' @section Objects from the Class
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @section Slots
#' @slot call an object of class "call" representing the matched call
#' @slot type a character string specifying the type of genetic algorithm used
#' @slot lower a vector providing for each decision variable the lower bounds of the search space in case of real-valued or permutation encoded optimisations. Formerly this slot was named min
#' @slot upper a vector providing for each decision variable the upper bounds of the search space in case of real-valued or permutation encoded optimizations. Formerly this slot was named max
#' @slot nBits a value specifying the number of bits to be used in binary encoded optimizations
#' @slot names a vector of character strings providing the names of decision variables (optional)
#' @slot popSize the population size
#' @slot front
#' @slot f
#' @slot iter the actual (or final) iteration of NSGA search
#' @slot run the number of consecutive generations without any improvement in the best fitness value before the NSGA is stopped
#' @slot maxiter the maximum number of iterations to run before the NSGA search is halted
#' @slot suggestions a matrix of user provided solutions and included in the initial population
#' @slot population the current (or final) population
#' @slot elitism the number of best fitness individuals to survive at each generation
#' @slot pcrossover the crossover probability
#' @slot pmutation the mutation probability
#' @slot optim a logical specifying whether or not a local search using general-purpose optimisation algorithms should be used
#' @slot crowdingDistance
#' @slot fitness the values of fitness function for the current (or final) population
#' @slot summary a matrix of summary statistics for fitness values at each iteration (along the rows)
#' @slot bestSol if keepBest = TRUE, the best solutions at each iteration
#' @slot fitnessValue the best fitness value at the final iteration
#' @slot solution the value(s) of the decision variables giving the best fitness at the final iteration.
#'
#' @examples
#' showClass("nsga3")
setClass(Class = "nsga3",
  representation(call = "language",
    type = "character",
    lower = "numberOrNAOrMatrix",
    upper = "numberOrNAOrMatrix",
    nBits = "numberOrNAOrMatrix",
    names = "character",
    popSize = "numeric",
    front = "numberOrNAOrMatrix",
    f = "list",
    iter = "numeric",
    run = "numeric",
    maxiter = "numeric",
    suggestions = "matrix",
    population = "numberOrNAOrMatrix",
    elitism = "numeric",
    pcrossover = "numeric",
    pmutation = "numberOrNAOrMatrix",
    optim = "logical",
    crowdingDistance = "numberOrNAOrMatrix",
    fitness = "numberOrNAOrMatrix",
    summary = "matrix",
    bestSol = "list",
    fitnessValue = "numeric",
    solution = "matrix"))
