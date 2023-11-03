#' R Multi-Objective Optimization Main Function
#'
#' Main function of rmoo, based on the parameters it will call the different
#' algorithms implemented in the package. Optimization algorithms will minimize
#' a fitness function. For more details of the algorithms
#' see [nsga2()], [nsga3()], [rnsga2()].
#'
#' Multi- and Many-Optimization of a fitness function using Non-dominated
#' Sorting Genetic Algorithms. The algorithms currently implemented by rmoo
#' are: NSGA-II, NSGA-III and R-NSGA-II
#'
#' The Non-dominated genetic algorithms II (NSGA-II) is a meta-heuristic proposed by
#' K. Deb, A. Pratap, S. Agarwal and T. Meyarivan in 2002. The purpose of the
#' algorithms is to find an efficient way to optimize multi-objectives functions
#' (two or more).
#'
#' The Non-dominated genetic algorithms III (NSGA-III) is a meta-heuristic proposed by
#' K. Deb and H. Jain in 2013.
#' The purpose of the algorithms is to find an efficient way to optimize
#' multi-objectives functions (more than three).
#'
#' The R-NSGA-II is a meta-heuristic proposed by K. Deb and J. Sundar in 2006.
#' It is a modification of NSGA-II based on reference points in which the
#' decision-maker supplies one or more preference points and a weight vector
#' that will guide the solutions towards regions desired by the user.
#'
#' @name rmoo_main
#'
#' @param ... argument in which all the values necessary for the configuration
#' will be passed as parameters. The user is encouraged to see the documentations
#' of [nsga2()], [rnsga2()], [nsga3()] in which the necessary parameters for each
#' algorithm are cited, in addition, the chosen strategy to execute must be
#' passed as an argument. This can be seen more clearly in the examples.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references Scrucca, L. (2017) On some extensions to 'GA' package: hybrid
#' optimisation, parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' @references Kalyanmoy Deb and J. Sundar. 2006. Reference point based
#' multi-objective optimization using evolutionary algorithms. In Proceedings of
#' the 8th annual conference on Genetic and evolutionary computation (GECCO '06).
#' Association for Computing Machinery, New York, NY, USA, 635–642.
#' doi: 10.1145/1143997.1144112
#'
#' K. Deb, A. Pratap, S. Agarwal and T. Meyarivan, 'A fast and
#' elitist multiobjective genetic algorithm: NSGA-II,' in IEEE Transactions on
#' Evolutionary Computation, vol. 6, no. 2, pp. 182-197, April 2002,
#' doi: 10.1109/4235.996017.
#'
#' K. Deb and H. Jain, "An Evolutionary Many-Objective Optimization
#' Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I:
#' Solving Problems With Box Constraints," in IEEE Transactions on Evolutionary
#' Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014,
#' doi: 10.1109/TEVC.2013.2281535.
#'
#' @return Returns an object of class nsga2-class, nsga3-class or rnsga2-class.
#' See [nsga2-class], [rnsga2-class], [nsga3-class] for a description of
#' available slots information.
#'
#' @seealso [nsga2()], [rnsga2()], [nsga3()]
#'
#' @examples
#' #Example 1
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
#' result <- rmoo(type = "real-valued",
#'                fitness = zdt1,
#'                algorithm = "NSGA-II",
#'                lower = c(0,0),
#'                upper = c(1,1),
#'                popSize = 100,
#'                nObj = 2,
#'                monitor = FALSE,
#'                maxiter = 500)
#'
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
#' #Define uniformly distributed reference points.
#' ref_points <- generate_reference_points(3,12)
#'
#' #Not Run
#' \dontrun{
#' result <- rmoo(type = "real-valued",
#'                 fitness = dtlz1,
#'                 algorithm = "NSGA-III",
#'                 lower = c(0,0,0),
#'                 upper = c(1,1,1),
#'                 popSize = 92,
#'                 nObj = 3,
#'                 reference_dirs = ref_points,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#'
#' @examples
#' #Example 3
#' #Two Objectives - Real Valued with Preference-guided
#' zdt2 <- function (x)
#' {
#'   if (is.null(dim(x))) {
#'     x <- matrix(x, nrow = 1)
#'   }
#'   n <- ncol(x)
#'   g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#'   return(cbind(x[, 1], g * (1 - (x[, 1]/g)^2)))
#' }
#'
#' #Define uniformly distributed reference points.
#' ref_points <- rbind(c(1.0, 0.0), c(0.0, 1.0), c(0.5, 0.5))
#'
#' #Not run
#' \dontrun{
#' result <- rmoo(type = "real-valued",
#'                fitness = zdt2,
#'                algorithm = "R-NSGA-II",
#'                lower = c(0,0),
#'                upper = c(1,1),
#'                reference_dirs = ref_points,
#'                popSize = 92,
#'                nObj = 2,
#'                monitor = FALSE,
#'                maxiter = 500)
#'
#' }
#'
#' @export
#' @aliases rmoo-main,rmoo-function
rmoo <- function(type = c("binary", "real-valued", "permutation", "discrete"),
                 algorithm = c("NSGA-II", "NSGA-III", "R-NSGA-II"),
                 fitness, ...,
                 lower, upper, nBits, nvars,
                 population = rmooControl(type)$population,
                 selection = rmooControl(type)$selection,
                 crossover = rmooControl(type)$crossover,
                 mutation = rmooControl(type)$mutation,
                 pcrossover = 0.8,
                 pmutation = 0.1,
                 popSize = 50,
                 maxiter = 100,
                 nObj = NULL,
                 names = NULL,
                 suggestions = NULL,
                 monitor = if (interactive()) rmooMonitor else FALSE,
                 parallel = FALSE,
                 summary = FALSE,
                 seed = NULL,
                 reference_dirs = NULL,
                 epsilon = 0.001,
                 normalization = NULL,
                 extreme_points_as_ref_dirs = FALSE,
                 weights = NULL)
{
  start_time <- Sys.time()
  call       <- match.call()
  type       <- match.arg(type, choices = eval(formals(rmoo)$type))
  algorithm  <- match.arg(algorithm, choices = eval(formals(rmoo)$algorithm))
  callArgs   <- list(...)

  if (!is.function(population)) population <- get(population)
  if (!is.function(selection)) selection <- get(selection)
  if (!is.function(crossover)) crossover <- get(crossover)
  if (!is.function(mutation)) mutation <- get(mutation)

  check_numeric_arg(nObj, "Objective number (nObj)", check_negative = TRUE)
  check_numeric_arg(maxiter, "Maximum number of iterations (maxiter)", check_negative = TRUE)
  check_function_arg(fitness, "Fitness function")
  check_probability_arg(pcrossover, "Probability of crossover (pcrossover)")
  check_probability_arg(pmutation, "Probability of mutation (pmutation)")
  check_algorithm_arg(nObj, algorithm, normalization, reference_dirs)
  if (popSize < 10) warning("The population size is less than 10.")

  switch(type,
         binary = {
           check_numeric_arg(nBits, "Number of bits (nBits)", check_negative = TRUE)
           nBits <- as.vector(nBits)[1]
           lower <- upper <- NA
           nvars <- nBits
           if (is.null(names))
             names <- paste0("x", 1:nvars)
         },
         `real-valued` = {
           lnames <- names(lower)
           unames <- names(upper)
           lower <- as.vector(lower)
           upper <- as.vector(upper)
           check_numeric_arg(length(lower), "Length of lower")
           check_numeric_arg(length(upper), "Length of upper")
           nBits <- NA
           nvars <- length(upper)
           if (is.null(names) & !is.null(lnames)) names <- lnames
           if (is.null(names) & !is.null(unames)) names <- unames
           if (is.null(names))
             names <- paste0("x", 1:nvars)
         },
         permutation = {
           check_numeric_arg(as.vector(lower), "Lower bounds (lower)")
           check_numeric_arg(as.vector(upper), "Upper bounds (upper)")
           lower <- as.vector(lower)[1]
           upper <- as.vector(upper)[1]
           nBits <- NA
           nvars <- length(seq.int(lower, upper))
           if (is.null(names))
             names <- paste0("x", 1:nvars)
         },
         discrete = {
           check_numeric_arg(as.vector(lower), "Lower bounds (lower)")
           check_numeric_arg(as.vector(upper), "Upper bounds (upper)")
           check_numeric_arg(nvars, "Number of decision variables (nvars)")
           lower <- as.vector(lower)[1]
           upper <- as.vector(upper)[1]
           nBits <- NA
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
    if (monitor) monitor <- rmooMonitor
  }
  if (is.null(monitor)) monitor <- FALSE

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

  i. <- NULL #dummy to trick R CMD check

  Fitness <- matrix(NA, nrow = popSize, ncol = nObj)

  fitnessSummary <- vector("list", maxiter)

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
         },
         discrete = {
           Pop <- P <- Q <- matrix(NA_integer_, nrow = popSize, ncol = nvars)
         }
  )

  ng <- min(nrow(suggestions), popSize)

  if (ng > 0) {
    Pop[1:ng, ] <- suggestions
  }

  object <- create_object_instance(algorithm, call, type, lower, upper, nBits,
                                   nvars, names, popSize, maxiter, suggestions,
                                   pcrossover, pmutation, Fitness,
                                   fitnessSummary, reference_dirs, nObj)

  if (maxiter == 0)
    return(object)

  if (popSize > ng) {
    Pop[(ng + 1):popSize, ] <- population(object)[1:(popSize - ng), ]
  }

  object@population <- Pop

  # Evaluate Solution Fitness
  Fitness <- evaluate_fitness(parallel, popSize, Fitness, fitness, Pop, `%DO%`, callArgs)

  object@population <- P <- Pop
  object@fitness <- p_fit <- Fitness

  # First Non-dominated Ranking
  out <- non_dominated_fronts(object)
  object@f <- out$fit
  object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)


  for (iter in seq_len(maxiter)) {
    # initialize the iteration counter
    object@iter <- iter

    # Selection operator
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

    # Crossover operator
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

    # Mutation operator
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

    # Evaluate Solution Fitness
    Fitness <- evaluate_fitness(parallel, popSize, Fitness, fitness, Pop, `%DO%`, callArgs)

    object@population <- Q <- Pop
    object@fitness <- q_fit <- Fitness

    # R = P U Q
    object@population <- Pop <- rbind(P, Q)
    object@fitness <- rbind(p_fit, q_fit)

    # Fast Non Dominated Sorting
    out <- non_dominated_fronts(object)
    object@f <- out$fit
    object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

    out <- optimization_process(object, algorithm, nObj, epsilon, weights, normalization, extreme_points_as_ref_dirs)

    object <- out$object
    Pop <- out$p_pop
    p_fit <- out$p_fit

    object@population <- P <- Pop
    object@fitness <- p_fit

    # Fast Non Dominated Sorting
    out <- non_dominated_fronts(object)
    object@f <- out$fit
    object@front <- matrix(unlist(out$fronts), ncol = 1, byrow = TRUE)

    if (summary) {
      fitnessSummary[[iter]] <- progress(object, callArgs)
      object@summary <- fitnessSummary
    } else {
      object@summary <- list(NULL)
    }

    if (is.function(monitor)) {
      # monitor(object = object, number_objective = nObj)
      monitor(object = object, callArgs)
    }

    # if (max(Fitness, na.rm = TRUE) >= maxFitness)
    #   break
    if (object@iter == maxiter)
      break
  }
  object@execution_time <- as.numeric(difftime(Sys.time() , start_time, units = "secs"))
  # solution <- object

  return(object)
}



# setClass("Person", representation(name = "character", age = "numeric", greet = "function"))
#
# setValidity("Person", function(object) {
#   if (nchar(object@name) == 0) {
#     return("Name must be non-empty")
#   }
#   TRUE
# })
#
# setGeneric("age", function(object, birthdate) {
#   standardGeneric("age")
# })
#
#
# setMethod("age", "Person", function(object, birthdate) {
#   today <- Sys.Date()
#   as.numeric(difftime(today, as.Date(birthdate), units = "weeks")) %/% 52
# })
#
#
# greetFunction <- function(person) {
#   cat(paste0("Hello, my name is ", person@name, " and I am ", person@age, " years old.\n"))
# }
#
# hadley <- new("Person", name = "Hadley", age = 31, greet = greetFunction)
#
# hadley$age("1979-07-30")
#




# r_nsga_iii <- function(){
#   get_ref_dirs_from_points <- function(ref_point, ref_dirs, mu = 0.1) {
#     n_obj <- ncol(ref_point)
#
#     val <- list()
#
#     n_vector <- rep(1 / sqrt(n_obj), n_obj)  # Normal vector of Das Dennis plane
#
#
#     point_on_plane <- diag(n_obj)[1, ]  # Point on Das-Dennis
#
#     for (i in 1:nrow(ref_point)) {
#       point <- ref_point[i,]
#       ref_dir_for_aspiration_point <- ref_dirs * mu # Copy of computed reference directions
#
#       cent <- colMeans(ref_dir_for_aspiration_point)  # Find centroid of shrunken reference points
#
#       # Project shrunken Das-Dennis points back onto original Das-Dennis hyperplane
#       intercept <- line_plane_intersection(rep(0, n_obj), point, point_on_plane, n_vector)
#       shift <- intercept - cent  # shift vector
#
#       ref_dir_for_aspiration_point <- sweep(ref_dir_for_aspiration_point, 2, shift, "+")
#
#       # If reference directions are located outside of first octant, redefine points onto the border
#       if (!all(ref_dir_for_aspiration_point > 0)) {
#         ref_dir_for_aspiration_point[ref_dir_for_aspiration_point < 0] <- 0
#         ref_dir_for_aspiration_point <- sweep(ref_dir_for_aspiration_point, 1, rowSums(ref_dir_for_aspiration_point), "/")
#       }
#       val <- c(val, list(ref_dir_for_aspiration_point))
#       #val <- c(val, ref_dir_for_aspiration_point)
#     }
#
#     val <- c(val, list(diag(n_obj)))  # Add extreme points
#     # val <- c(val, diag(n_obj))  # Add extreme points
#     return(do.call(rbind, val))
#     # return(array(unlist(val), dim = c(length(val), n_obj)))
#   }
#
#
#   line_plane_intersection <- function(l_zero, l_one, p_zero, p_no, epsilon = 1e-6) {
#
#     l <- l_one - l_zero
#     dot <- sum(l * p_no)
#
#     if (abs(dot) > epsilon) {
#       w <- p_zero - l_zero
#       d <- sum(w * p_no) / dot
#       l <- l * d
#       return(l_zero + l)
#     } else {
#       ref_proj <- l_one - sum((l_one - p_zero) * p_no) * p_no
#
#       return(ref_proj)
#     }
#   }
# }

# ref_point <- rbind(c(1.0, 0.5, 0.2),c(0.3, 0.2, 0.6))
# ref_dirs <- generate_reference_points(3,12)
# pop_per_ref_point=50
# mu <- 0.1

# get_ref_dirs_from_points(ref_point, ref_dirs, mu = 0.1)
