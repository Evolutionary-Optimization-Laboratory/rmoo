#'Virtual Parent Class Algorithm, it will use when other algorithms are implemented.
#' Equivalent to a Abstract class in other languages.
# @export
setClass("algorithm", contains = "VIRTUAL")
#setClassUnion("algorithm", "nsga")

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
# @export
setClass(Class = "nsga",
         slots = list(call = "language",
                     type = "character",
                     lower = "OptNumberOrMatrix",
                     upper = "OptNumberOrMatrix",
                     nBits = "OptNumberOrMatrix",
                     names = "character",
                     popSize = "numeric",
                     front = "OptNumberOrMatrix",
                     f = "list",
                     iter = "numeric",
                     run = "numeric",
                     maxiter = "numeric",
                     suggestions = "matrix",
                     population = "OptNumberOrMatrix",
                     pcrossover = "numeric",
                     pmutation = "OptNumberOrMatrix",
                     fitness = "OptNumberOrMatrix",
                     summary = "list",
                     solution = "matrix",
                     fitnessValue = "OptNumberOrMatrix"),
         contains = "algorithm"
)


#' Virtual Class 'nsga1 - Simple Class for subassigment Values'
#'
#' The class 'nsga1' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
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
         slots = list(dumFitness = "OptNumberOrMatrix",
                      dShare = "numeric",
                      deltaDummy = "numeric"),
         contains = "nsga"
)

# validNSGAIObject <- function(object) {
#   if(length(object@x) == length(object@y)) TRUE
#   else paste("Unequal x,y lengths: ", length(object@x), ", ",
#              length(object@y), sep="")
# }
#
# setValidity("nsga1", validNSGAIObject)

#' Virtual Class 'nsga2 - Simple Class for subassigment Values'
#'
#' The class 'nsga2' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @slot crowdingDistance Crowding-comparison approach to estiate of the
#' perimeter of the cuboid formed by using the nearest neighbors as the vertices.
#'
#' @examples
#' showClass('nsga2')
#' @export
setClass(Class = "nsga2",
         slots = list(crowdingDistance = "OptNumberOrMatrix"),
         contains = "nsga"
)

# validNSGAIIObject <- function(object) {
#   if(length(object@x) == length(object@y)) TRUE
#   else paste("Unequal x,y lengths: ", length(object@x), ", ",
#              length(object@y), sep="")
# }
#
# setValidity("nsga2", validNSGAIIObject)

#' Virtual Class 'nsga3 - Simple Class for subassigment Values'
#'
#' The class 'nsga3' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
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
         slots = list(ideal_point = "OptNumberOrMatrix",
                      worst_point = "OptNumberOrMatrix",
                      smin = "OptNumberOrMatrix",
                      extreme_points = "OptNumberOrMatrix",
                      worst_of_population = "OptNumberOrMatrix",
                      worst_of_front = "OptNumberOrMatrix",
                      nadir_point = "OptNumberOrMatrix",
                      reference_points = "OptNumberOrMatrix"),
         contains = "nsga"
)

#
# validNSGAIIIObject <- function(object) {
#   if(length(object@x) == length(object@y)) TRUE
#   else paste("Unequal x,y lengths: ", length(object@x), ", ",
#              length(object@y), sep="")
# }
#
# setValidity("nsga3", validNSGAIIIObject)
