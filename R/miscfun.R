#' @export
nsgaMonitor <- function(object, number_objectives, ...) {
  if (!requireNamespace("rgl", quietly = TRUE)){
    stop("packages 'rgl' required for Monitor, please install it!")
  }
  fitness <- object@fitness
  iter <- object@iter
  cl <- rainbow(object@popSize)

  if (number_objectives == 3) {
    rgl::plot3d(fitness)
    rgl::bgplot3d({
      plot.new()
      title(main = paste(class(object)[1], "Iter: ", iter), line = 3)
    })
    plot(seq(number_objectives), object@fitness[1, ],
      col = cl[1], type = "l", main = paste(class(object)[1],
        "Iter: ", iter),
      ylim = c(min(object@fitness), max(object@fitness)),
      xlab = "Objective_No", ylab = "Objective_Value")
    for (i in 2:(object@popSize)) {
      lines(seq(number_objectives),
        object@fitness[i, ], col = cl[i], type = "l")
    }

  } else if (number_objectives == 2) {
    first_front <- fitness[object@f[[1]], ]
    opar <- par('mar','xpd')
    on.exit(par(opar))
    par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
    plot(fitness[, 1], fitness[, 2],
      col = "green", pch = 20, main = paste(class(object)[1],
        "Iter: ", iter),
      xlim = range(fitness[, 1]), ylim = range(fitness[, 2]),
      xlab = "f_1", ylab = "f_2")
    lines(first_front[, 1][order(first_front[, 1])],
      first_front[, 2][order(first_front[, 1])],
      xlim = range(first_front[, 1]),
      ylim = range(first_front[, 2]),
      xlab = "f_1", ylab = "f_2",
      col = "red", type = "l", pch = 12, main = "Pareto Front")
    legend("topright", inset = c(-0.8, 0),
      legend = c("Population", "Pareto Optimal"), pch = c(19, NA),
      title = "Values", lwd = c(NA, 2),
      col = c("green", "red"), y.intersp = 1.5)


  } else if (number_objectives > 3) {
    plot(seq(number_objectives), object@fitness[1, ],
      col = cl[1], type = "l", main = paste(class(object)[1],
        "Iter: ", iter),
      ylim = c(min(object@fitness), max(object@fitness)),
      xlab = "Objective_No", ylab = "Objective_Value")
    for (i in 2:(object@popSize)) {
      lines(seq(number_objectives), object@fitness[i, ], col = cl[i], type = "l")
    }

  }
}

#' @export
summary.rmoo <- function(object, ...) {
  algorithm <- class(object)[1]
  if(algorithm == "nsga1"){
    out <- nsgaSummary(object)
  }
  else if(algorithm == "nsga2"){
    out <- nsgaiiSummary(object)
  }
  else if(algorithm == "nsga3"){
    out <- nsgaiiiSummary(object)
  }
  #class(out) <- "summary"
  return(out)
}

#colMax <- function(data) apply(data, 2, max)

#Summary for NSGA Algorithm
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
