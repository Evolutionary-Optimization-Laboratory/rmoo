#' Virtual Class 'numberOrNAOrMatrix - Simple Class for subassigment Values'
#'
#' The class 'numberOrNAOrMatrix' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @examples
#' showClass('numberOrNAOrMatrix')

# A new class is created with the union of numeric, logical and matrix
#' @export
setClassUnion("numberOrNAOrMatrix",  members = c("numeric", "logical", "matrix", "NULL"))


#' Virtual Parent Class Algorithm
#'
#' It will use when other algorithms are implemented. Equivalent to a Abstract
#' class in other languages.
#' @export
setClass("algorithm", contains = "VIRTUAL")

#' Virtual Class 'nsga'
#'
#' The 'nsga' class is the parent superclass of the \linkS4class{nsga1},
#' \linkS4class{nsga2},  and \linkS4class{nsga3} classes
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
         slots = list(call = "language",
                     type = "character",
                     lower = "numberOrNAOrMatrix",
                     upper = "numberOrNAOrMatrix",
                     nBits = "numberOrNAOrMatrix",
                     nvars = "numberOrNAOrMatrix",
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
                     fitness = "numberOrNAOrMatrix",
                     summary = "list",
                     solution = "matrix",
                     fitnessValue = "numberOrNAOrMatrix",
                     execution_time = "numberOrNAOrMatrix"),
         contains = c("VIRTUAL", "algorithm"),
         #contains = "algorithm"
)


#' Class 'nsga1'
#'
#' The class 'nsga1' is instantiated within the execution of rmoo and will be
#' returned as a result of it. All data generated during execution will be
#' stored in it.
#'
#' @slot dumFitness a large dummy fitness value assigned to individuals from
#' the nondominated front.
#' @slot dShare the maximun phenotypic distance allowed between any two
#' individuals to become members of a niche.
#' @slot deltaDummy value to decrease the dummy fitness of individuals by
#' non-dominated fronts.
#'
#' @examples
#' showClass('nsga1')
#' @export
setClass(Class = "nsga1",
         slots = list(dumFitness = "numberOrNAOrMatrix",
                      dShare = "numeric",
                      deltaDummy = "numeric"),
         contains = "nsga"
)

#' Class 'nsga2'
#'
#' The class 'nsga2' is instantiated within the execution of rmoo and will be
#' returned as a result of it. All data generated during execution will be
#' stored in it.
#'
#' @slot crowdingDistance Crowding-comparison approach to estimate of the
#' perimeter of the cuboid formed by using the nearest neighbors as the vertices.
#'
#' @examples
#' showClass('nsga2')
#' @export
setClass(Class = "nsga2",
         slots = list(crowdingDistance = "numberOrNAOrMatrix"),
         contains = "nsga"
)


#' Class 'rnsga2'
#'
#' The class 'rnsga2' is instantiated within the execution of rmoo and will be
#' returned as a result of it. All data generated during execution will be
#' stored in it.
#'
#' @slot crowdingDistance Crowding-comparison approach to estimate of the
#' perimeter of the cuboid formed by using the nearest neighbors as the vertices.
#' @slot reference_points R-NSGA-II uses a set of reference points defined by the user to
#' ensure diversity in obtained solutions.
#' @slot extreme_points are selected using the ASF in the ([PerformScalarizing()]).
#' Necessary in the  nadir point generation.
#' @slot smin Index used to obtain the extreme points.
#'
#' @examples
#' showClass('rnsga2')
#' @export
setClass(Class = "rnsga2",
         slots = list(crowdingDistance = "numberOrNAOrMatrix",
                      reference_points = "numberOrNAOrMatrix",
                      extreme_points = "numberOrNAOrMatrix",
                      smin = "numberOrNAOrMatrix"),
         contains = "nsga2")


#' Class 'nsga3'
#'
#' The class 'nsga3' is instantiated within the execution of rmoo and will be
#' returned as a result of it. All data generated during execution will be
#' stored in it.
#'
#' @slot ideal_point Nadir point estimate used as lower bound in normalization.
#' @slot worst_point Worst point generated over generations.
#' @slot smin Index used to obtain the extreme points.
#' @slot extreme_points are selected using the ASF in the ([PerformScalarizing()]).
#' Necessary in the  nadir point generation.
#' @slot worst_of_population The worst individuals generated by objectives in
#' the current generation.
#' @slot worst_of_front The worst individuals in the first front generated by
#' objectives in the current generation.
#' @slot nadir_point Nadir point estimate used as upper bound in normalization.
#' @slot reference_points NSGA-III uses a predefined set of reference points to
#' ensure diversity in obtained solutions.
#' The chosen refenrece points can be predefined in structured manner or
#' supplied by the user. We use the Das and Dennis procedure.
#'
#' @examples
#' showClass('nsga3')
#' @export
setClass(Class = "nsga3",
         slots = list(ideal_point = "numberOrNAOrMatrix",
                      worst_point = "numberOrNAOrMatrix",
                      smin = "numberOrNAOrMatrix",
                      extreme_points = "numberOrNAOrMatrix",
                      worst_of_population = "numberOrNAOrMatrix",
                      worst_of_front = "numberOrNAOrMatrix",
                      nadir_point = "numberOrNAOrMatrix",
                      reference_points = "numberOrNAOrMatrix"),
         contains = "nsga"
)

# Agregar a la clases NSGA
# Atributos necesarios para el elitismo de las soluciones
# p_fit, q_fit
# p_pop, q_pop


# Agregar a la clases R-NSGA-II
# smin
