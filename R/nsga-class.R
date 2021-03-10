#' Virtual Class 'nsga - Simple Class for subassigment Values'
#'
#' The class 'nsga' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @slot call an object of class 'call' representing the matched call.
#' @slot type a character string specifying the type of genetic algorithm used.
#' @slot lower a vector providing for each decision variable the lower bounds of
#' the search space in case of real-valued or permutation encoded optimisations.
#' @slot upper a vector providing for each decision variable the upper bounds of
#' the search space in case of real-valued or permutation encoded optimizations.
#' @slot nBits a value specifying the number of bits to be used in binary
#' encoded optimizations.
#' @slot names a vector of character strings providing the names of decision
#' variables (optional).
#' @slot popSize the population size.
#' @slot front Rank of individuals on the non-dominated front.
#' @slot f Front of individuals on the non-dominated front.
#' @slot iter the actual (or final) iteration of NSGA search.
#' @slot run the number of consecutive generations without any improvement in
#' the best fitness value before the NSGA is stopped.
#' @slot maxiter the maximum number of iterations to run before the NSGA search
#' is halted.
#' @slot suggestions a matrix of user provided solutions and included in the
#' initial population.
#' @slot population the current (or final) population.
#' @slot pcrossover the crossover probability.
#' @slot pmutation the mutation probability.
#' @slot dumFitness a large dummy fitness value assigned to individuals from
#' the nondominated front.
#' @slot dShare the maximun phenotypic distance allowed between any two
#' individuals to become members of a niche.
#' @slot deltaDummy value to decrease the dummy fitness of individuals by
#' non-dominated fronts.
#' @slot fitness the values of fitness function for the current (or final)
#' population.
#' @slot summary a matrix of summary statistics for fitness values at each
#' iteration (along the rows).
#' @slot fitnessValue the best fitness value at the final iteration.
#' @slot solution the value(s) of the decision variables giving the best fitness
#' at the final iteration.
#'
#' @examples
#' showClass('nsga')
#' @export
setClass(Class = "nsga",
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
     pcrossover = "numeric",
     pmutation = "numberOrNAOrMatrix",
     dumFitness = "numberOrNAOrMatrix",
     dShare = "numeric",
     deltaDummy = "numeric",
     fitness = "numberOrNAOrMatrix",
     summary = "list",
     fitnessValue = "numeric",
     solution = "matrix")
)
