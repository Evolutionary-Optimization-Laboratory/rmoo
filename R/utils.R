# @export
check_numeric_arg <- function(arg=NULL, name, check_negative = FALSE) {
  if (is.null(arg)) stop(paste("Please, define the", name))
  if (!is.numeric(arg))
    stop(paste(name, "must be a numeric value."))
  if ((arg %% 1 != 0))
    stop(paste(name, "must be a non-negative integer."))
  if (!check_negative && arg < 0)
    stop(paste(name, "must not be negative."))
}

# @export
check_probability_arg <- function(arg, name) {
  if (!is.numeric(arg))
    stop(paste(name, "must be a numeric value."))
  if (arg < 0 || arg > 1)
    stop(paste(name, "must be a numeric value in [0, 1]."))
}

# @export
check_function_arg <- function(arg=NULL, name) {
  if (is.null(arg)) stop(paste("Please, define the", name))
  if (!is.function(arg))
    stop(paste(name, "must be a function."))
}

# @export
check_matrix_arg <- function(arg, name) {
  if (is.null(arg)) stop(paste("Please, define the", name))
  if (!is.matrix(arg))
    stop(paste(name, "must be a matrix."))
}

# @export
check_algorithm_arg <- function(nObj, algorithm, normalization=NULL, reference_dirs=NULL){
  if (algorithm == "NSGA-III"){
    check_matrix_arg(reference_dirs, "Reference points")
    if (ncol(reference_dirs) != nObj) {
      stop("Dimensionality of reference points must be equal to the number of objectives")
    }
  }
  if (algorithm == "R-NSGA-II"){
    check_matrix_arg(reference_dirs, "Reference points")
    if (ncol(reference_dirs) != nObj) {
      stop("Dimensionality of reference points must be equal to the number of objectives")
    }
    if (is.null(normalization)) stop(paste("Please, define the Normalization type ('ever', 'front', 'no')"))

    normalization_values <- c("ever", "front", "no")
    if (!(normalization %in% normalization_values)) {
      stop("Normalization of the crowding distance must be 'ever', 'front' or 'no'.")
    }
  }
}

# @export
create_object_instance <- function(algorithm, call, type, lower,
                                   upper, nBits, nvars, names, popSize,
                                   maxiter, suggestions, pcrossover,
                                   pmutation, Fitness, fitnessSummary,
                                   reference_dirs, nObj){
  if (algorithm == "NSGA-II") {
    object <- new("nsga2",
                  call = call, type = type, lower = lower, upper = upper,
                  nBits = nBits, nvars = nvars,
                  names = if (is.null(names))
                    character()
                  else names,
                  popSize = popSize, front = matrix(), f = list(), iter = 0,
                  run = 1, maxiter = maxiter, suggestions = suggestions,
                  population = matrix(), pcrossover = pcrossover,
                  pmutation = if (is.numeric(pmutation))
                    pmutation
                  else NA,
                  crowdingDistance = matrix(NA_real_, nrow = popSize),
                  fitness = Fitness, summary = fitnessSummary)
  } else if (algorithm == "R-NSGA-II") {
    object <- new("rnsga2",
                  call = call, type = type, lower = lower, upper = upper,
                  nBits = nBits, nvars = nvars,
                  names = if (is.null(names))
                    character()
                  else names,
                  popSize = popSize, front = matrix(), f = list(), iter = 0,
                  run = 1, maxiter = maxiter, suggestions = suggestions,
                  population = matrix(), pcrossover = pcrossover,
                  pmutation = if (is.numeric(pmutation))
                    pmutation
                  else NA,
                  crowdingDistance = c(), fitness = Fitness,
                  reference_points = reference_dirs, extreme_points = matrix(),
                  smin =  rep(NA, nObj), summary = fitnessSummary)
  } else if (algorithm == "NSGA-III") {
    object <- new("nsga3",
                  call = call, type = type, lower = lower, upper = upper,
                  nBits = nBits, nvars = nvars,
                  names = if (is.null(names))
                    character()
                  else names,
                  popSize = popSize, front = matrix(), f = list(), iter = 0,
                  run = 1, maxiter = maxiter, suggestions = suggestions,
                  population = matrix(), ideal_point = NA, worst_point = NA,
                  smin = rep(NA, nObj), extreme_points = matrix(),
                  worst_of_population = rep(NA, nObj), worst_of_front = rep(NA, nObj),
                  nadir_point = rep(NA, nObj), pcrossover = pcrossover,
                  pmutation = if (is.numeric(pmutation))
                    pmutation
                  else NA,
                  reference_points = reference_dirs, fitness = Fitness, summary = fitnessSummary)
  } else {
    stop("Invalid algorithm specified. Supported algorithms: 'NSGA-II', 'R-NSGA-II', 'NSGA-III'")
  }

  return(object)
}

# @export
evaluate_fitness <- function(parallel, popSize, Fitness, fitness, Pop, `%DO%`, callArgs) {
  if (!parallel) {
    for (i in seq_len(popSize)) {
      if (is.na(Fitness[i])) {
        fit <- do.call(fitness, c(list(Pop[i, ]), callArgs))
        Fitness[i, ] <- fit
      }
    }
  } else {
    Fitness <- foreach(i. = seq_len(popSize), .combine = "rbind") %DO%
      {
        if(is.na(Fitness[i.])) do.call(fitness, c(list(Pop[i.,]), callArgs))
        else Fitness[i.,]
      }
  }
  return(Fitness)
}

# @export
optimization_process <- function(object, algorithm, nObj, epsilon, weights, normalization, extreme_points_as_ref_dirs) {
  if (algorithm == "NSGA-II") {
    out <- nsga_ii(object, nObj)
  } else if (algorithm == "R-NSGA-II") {
    out <- r_nsga_ii(object, epsilon, weights, normalization, extreme_points_as_ref_dirs)
  } else if (algorithm == "NSGA-III") {
    out <- nsga_iii(object, nObj)
  } else {
    stop("Invalid algorithm specified. Supported algorithms: 'NSGA-II', 'R-NSGA-II', 'NSGA-III'")
  }
  return(out)
}



get_relation <- function(a, b, cva = NULL, cvb = NULL) {
  if (!is.null(cva) && !is.null(cvb)) {
    if (cva < cvb) {
      return(1)
    } else if (cvb < cva) {
      return(-1)
    }
  }

  val <- 0
  for (i in 1:length(a)) {
    if (a[i] < b[i]) {
      if (val == -1) {
        return(0)
      }
      val <- 1
    } else if (b[i] < a[i]) {
      if (val == 1) {
        return(0)
      }
      val <- -1
    }
  }
  return(val)
}


compare <- function(a, a_val, b, b_val, method, return_random_if_equal = TRUE) {
  if (method == 'larger_is_better') {
    if (a_val > b_val) {
      return(a)
    } else if (a_val < b_val) {
      return(b)
    } else {
      if (return_random_if_equal) {
        return(sample(c(a, b), 1))
      } else {
        return(NULL)
      }
    }
  } else if (method == 'smaller_is_better') {
    if (a_val < b_val) {
      return(a)
    } else if (a_val > b_val) {
      return(b)
    } else {
      if (return_random_if_equal) {
        return(sample(c(a, b), 1))
      } else {
        return(NULL)
      }
    }
  }
}

random_permutations <- function(n, l, concat = TRUE) {
  P <- list()
  for (i in 1:n) {
    P[[i]] <- sample(1:l, l)
  }
  if (concat) {
    P <- unlist(P)
  }
  return(P)
}

