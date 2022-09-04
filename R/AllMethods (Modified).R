setGeneric("getCrowdingDistance", function(obj) standardGeneric("getCrowdingDistance"))
setMethod("getCrowdingDistance", "nsga2", function(obj) print(obj@crowdingDistance))

setGeneric("getDummyFitness", function(obj) standardGeneric("getDummyFitness"))

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

# -----------------------------------------------------------------------------

setGeneric("getPopulation", function(obj) standardGeneric("getPopulation"))

setMethod("getPopulation", "nsga",
          function(obj) {
            print(obj@population)
            n_value <- ncol(obj@population)
            population <- data.frame(obj@population)
            colnames(population) <- sprintf("Val_%s",seq(n_value))
            return(invisible(population))
          }
)

setGeneric("getFitness", function(obj) standardGeneric("getFitness"))

setMethod("getFitness", "nsga",
          function(obj) {
            print(obj@fitness)
            n_value <- ncol(obj@fitness)
            fitness <- data.frame(obj@fitness)
            colnames(fitness) <- sprintf("Fit_%s",seq(n_value))
            return(invisible(fitness))
          }
)

# -----------------------------------------------------------------------------

setMethod("print", "nsga",
          function(x=object, y="missing", ...) {
            # algorithm <- class(object)[1]
            # Print
            cat("Slots Configuration:\n")
            print(as.list(slotNames(x)))
            cat("\n#========================================#\n")
            cat("\nTotal iterations: ", x@iter)
            cat("\nPopulation size: ", x@popSize)
            cat("\nLower Bounds: ", x@lower)
            cat("\nLower Bounds:  ", x@upper)
            cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
            cat("\n#========================================#\n")
          }
)

setMethod("print", "nsga1",
          function(x=object, y="missing", ...) {
            algorithm <- class(x)[1]
            # Print
            cat("\nSlots Configuration:\n")
            print(as.list(slotNames(x)))
            cat("\n#========================================#\n")
            cat("\nTotal iterations: ", x@iter)
            cat("\nPopulation size: ", x@popSize)
            cat("\nLower Bounds: ", x@lower)
            cat("\nLower Bounds:  ", x@upper)
            cat("\nDelta Distance (dShare):  ", x@dShare)
            cat("\nDistance of sharing function:  ", x@deltaDummy)
            cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
            cat("\n#========================================#\n")

          }
)

setMethod("print", "nsga3",
          function(x=object, y="missing", ...) {
            # algorithm <- class(object)[1]
            # Print
            cat("Slots Configuration:\n")
            print(as.list(slotNames(x)))
            cat("\n#========================================#\n")
            cat("\nTotal iterations: ", x@iter)
            cat("\nPopulation size: ", x@popSize)
            cat("\nLower Bounds: ", x@lower)
            cat("\nUpper Bounds:  ", x@upper)
            cat("\nEstimated Ideal Point:  ", x@ideal_point)
            cat("\nEstimated Worst Point:  ", x@worst_point)
            cat("\nEstimated Nadir Point:  ", x@nadir_point)
            cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
            cat("\n#========================================#\n")
          }
)

# -----------------------------------------------------------------------------

setMethod("summary", "nsga3",
          function(object, ...){
              callArgs <- list(...)
              nullRP <- is.null(callArgs$reference_dirs)

              # Calculate information for summary

              first <- object@f[[1]]
              first_front_fit <- object@fitness[first, ]
              first_front_pop <- object@population[first, ]
              nadir_point <- object@nadir_point

              #first_dum <- object@dumFitness[first, ] for nsga1 summary method

              if("ecr" %in% rownames(utils::installed.packages())){
                if (nullRP) {
                  cat("Warning: reference points not provided:\n
                      value necessary to evaluate GD and IGD.")

                } else{
                  gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
                  igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
                }
              }

              if("emoa" %in% rownames(utils::installed.packages())){
                if(nullRP) {
                  cat("Warning: reference points not provided:\n
                      using the maximum in each dimension to evaluate Hypervolumen")
                  reference_point <- nadir_point
                } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
                hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
              }

              cat("\nSummary of NSGA-III run")
              cat("\n#====================================")
              cat("\nTotal Objectives evaluated: ", ncol(object@fitness))
              cat("\nTotal iterations: ", object@iter)
              cat("\nPopulation size: ", object@popSize)
              #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
              cat("\nNondominated points found: ", length(first),
                  paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
                  "of total)")
              cat("\nEstimated ideal point: ", round(object@ideal_point, 3))
              cat("\nEstimated nadir point: ", round(object@nadir_point, 3))
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

setMethod("summary", "nsga2",
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
                cat("Warning: reference points not provided:\n
                      value necessary to evaluate GD and IGD.")

              } else{
                gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
                igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
              }
            }

            if("emoa" %in% rownames(utils::installed.packages())){
              if(nullRP) {
                cat("Warning: reference points not provided:\n
                      using the maximum in each dimension to evaluate Hypervolumen")
                reference_point <- nadir_point
              } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
              hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
            }

            cat("\nSummary of NSGA-II run")
            cat("\n#====================================")
            cat("\nTotal function evaluations: ", ncol(object@fitness))
            cat("\nTotal iterations: ", object@iter)
            cat("\nPopulation size: ", object@popSize)
            #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
            cat("\nNondominated points found: ", length(first),
                paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
                "of total)")
            cat("\nCrowding distance bounds: ", c(max(object@crowdingDistance), min(object@crowdingDistance)))
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
                cat("Warning: reference points not provided:\n
                      value necessary to evaluate GD and IGD.")

              } else{
                gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
                igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
              }
            }

            if("emoa" %in% rownames(utils::installed.packages())){
              if(nullRP) {
                cat("Warning: reference points not provided:\n
                      using the maximum in each dimension to evaluate Hypervolumen")
                reference_point <- nadir_point
              } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
              hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
            }

            cat("\nSummary of NSGA-I run")
            cat("\n#====================================")
            cat("\nTotal function evaluations: ", ncol(object@fitness))
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

# -----------------------------------------------------------------------------

nsga3_object <- new("nsga3",
                    call = nsga3@call,
                    type = nsga3@type,
                    lower = nsga3@lower,
                    upper = nsga3@upper,
                    nBits = nsga3@nBits,
                    names = nsga3@names,
                    popSize = nsga3@popSize,
                    front = nsga3@front,
                    f = nsga3@f,
                    iter = nsga3@iter,
                    run = nsga3@run,
                    maxiter = nsga3@maxiter,
                    suggestions = nsga3@suggestions,
                    population = nsga3@population,
                    ideal_point = nsga3@ideal_point, #Agregar en nsga3-class
                    worst_point = nsga3@worst_point, #Agregar en nsga3-class
                    smin = nsga3@smin,
                    extreme_points = nsga3@extreme_points, #Agregar en nsga3-class
                    worst_of_population = nsga3@worst_of_population, #Agregar en nsga3-class
                    worst_of_front = nsga3@worst_of_front, #Agregar en nsga3-class
                    nadir_point = nsga3@nadir_point,
                    pcrossover = nsga3@pcrossover,
                    pmutation = nsga3@pmutation,
                    reference_points = nsga3@reference_points, #Agregar en nsga3-class
                    fitness = nsga3@fitness,
                    summary = nsga3@summary)

nsga2_object <- new("nsga2",
                    call = nsga2@call,
                    type = nsga2@type,
                    lower = nsga2@lower,
                    upper = nsga2@upper,
                    nBits = nsga2@nBits,
                    names = nsga2@names,
                    popSize = nsga2@popSize,
                    front = nsga2@front,
                    f = nsga2@f,
                    iter = nsga2@iter,
                    run = nsga2@run,
                    maxiter = nsga2@maxiter,
                    suggestions = nsga2@suggestions,
                    population = nsga2@population,
                    pcrossover = nsga2@pcrossover,
                    pmutation = nsga2@pmutation,
                    crowdingDistance = nsga2@crowdingDistance,
                    fitness = nsga2@fitness,
                    summary = nsga2@summary)

nsga1_object <- new("nsga1",
                    call = nsga1@call,
                    type = nsga1@type,
                    lower = nsga1@lower,
                    upper = nsga1@upper,
                    nBits = nsga1@nBits,
                    names = nsga1@names,
                    popSize = nsga1@popSize,
                    front = nsga1@front,
                    f = nsga1@f,
                    iter = nsga1@iter,
                    run = nsga1@run,
                    maxiter = nsga1@maxiter,
                    suggestions = nsga1@suggestions,
                    population = nsga1@population,
                    pcrossover = nsga1@pcrossover,
                    pmutation = nsga1@pmutation,
                    dumFitness = nsga1@dumFitness,
                    dShare = nsga1@dShare,
                    deltaDummy = nsga1@deltaDummy,
                    fitness = nsga1@fitness,
                    summary = nsga1@summary)

# -----------------------------------------------------------------------------

heat_map <- function(object, ...){
  if(!all(requireNamespace("ggplot2", quietly = TRUE),
          requireNamespace("reshape2", quietly = TRUE),
          requireNamespace("dplyr", quietly = TRUE)))
    stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for heat map plotting!")
  callArgs <- list(...)
  individual <- callArgs$individual
  fitness <- object@fitness

  if(is.null(individual))
    stop("Please, define a vector with the individuals to plot")

  if (length(individual) > 10) {
    cat("Warning: Heatmap plot with more than 10 individuals will not be displayed correctly.\n
                  The plot will still be displayed.")
  }

  fitness <- fitness[individual,]

  if (is.null(dim(fitness))){
    fitness <- t(matrix(fitness))
  }

  nObj <- ncol(fitness)
  colnames(fitness) <- sprintf("f_%s",seq(nObj))
  fitness <- reshape2::melt(fitness)
  fitness <- dplyr::rename(fitness,
                           'Pop' = Var1,
                           'Objective_Value' = value,
                           'Objective_No' = Var2)
  ggplot2::ggplot(fitness, aes(x = Objective_No,
                               y = Pop,
                               fill = Objective_Value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_continuous(labels = unique(as.character(fitness$Pop)),
                                breaks = unique(fitness$Pop))
}

polar <- function(object, ...){
  if(!all(requireNamespace("ggplot2", quietly = TRUE),
          requireNamespace("reshape2", quietly = TRUE),
          requireNamespace("dplyr", quietly = TRUE)))
    stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for polar coordinate plotting!")

  callArgs <- list(...)
  individual <- callArgs$individual
  fitness <- object@fitness

  if(is.null(individual))
    stop("Please, define a vector with the individuals to plot")

  if (length(individual) > 10) {
    cat("Warning: Polar Coordinate plot with more than 10 individuals will not be displayed correctly.\n
                  The plot will still be displayed.")
  }

  fitness <- fitness[individual,]

  if (is.null(dim(fitness))){
    fitness <- t(matrix(fitness))
  }

  nObj <- ncol(fitness)
  colnames(fitness) <- sprintf("f_%s",seq(nObj))
  fitness <- reshape2::melt(fitness)
  fitness <- dplyr::rename(fitness,
                           'Pop' = Var1,
                           'Objective_Value' = value,
                           'Objective_No' = Var2)
  ggplot2::ggplot(fitness, aes(x = Objective_No,
                               y = Objective_Value,
                               fill = Objective_No)) +
    ggplot2::geom_bar(width = 1, stat="identity") +
    ggplot2::coord_polar() + ggplot2::theme_light() +
    ggplot2::facet_wrap(~Pop, nrow = 1)
}

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

.get.plotting <- function(x, y="missing",
                          type="scatter", ...){
  switch(type,
         "scatter" = {
           scatter(x, ...)
         },
         "pcp"  = {
           pcp(x, ...)
         },
         "heatmap" = {
           heat_map(x, ...)
         },
         "polar" = {
           polar(x, ...)
         }
  )
}

setMethod("plot", signature(x="nsga1", y="missing"), .get.plotting)
setMethod("plot", signature(x="nsga2", y="missing"), .get.plotting)
setMethod("plot", signature(x="nsga3", y="missing"), .get.plotting)

# -----------------------------------------------------------------------------

if (!isGeneric("progress"))
  setGeneric("progress", function(object, ...) standardGeneric("progress"))

.nsga1.progress <- function(object, ...) {
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
  return(invisible(result))
}

.nsga2.progress <- function(object, ...) {
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

  return(invisible(result))
}


.nsga3.progress <- function(object, ...) {
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
  return(invisible(result))
}


setMethod("progress", "nsga1", .nsga1.progress)
setMethod("progress", "nsga2", .nsga2.progress)
setMethod("progress", "nsga3", .nsga3.progress)




getCrowdingDistance(nsga2_object)
getDummyFitness(nsga1_object)
getPopulation(nsga1_object)
getPopulation(nsga2_object)
getPopulation(nsga3_object)
getFitness(nsga1_object)
getFitness(nsga2_object)
getFitness(nsga3_object)
print(nsga1_object)
print(nsga2_object)
print(nsga3_object)
summary(nsga1_object)
summary(nsga2_object)
summary(nsga3_object)
plot(nsga1_object) #Default plot scatter graph
plot(nsga2_object) #Default plot scatter graph
plot(nsga3_object) #Default plot scatter graph

plot(nsga1_object, type = "pcp")
plot(nsga1_object, type = "heatmap", individual= c(1:3))
plot(nsga1_object, type = "polar", individual= c(1:3))







# -----------------------------------------------------------------------------
# setMethod("getPopulation", "nsga",
#           function(obj) {
#               algorithm <- toupper(class(obj)[1])
#               algorithm <- gsub('NSGA1', 'NSGA-I',
#                                 gsub('NSGA2', 'NSGA-II',
#                                      gsub('NSGA3', 'NSGA-III',
#                                           algorithm)))
#               cat(algorithm, "Population: \n")
#               cat("\n#========================================#\n")
#               print(obj@population)
#               n_value <- ncol(obj@population)
#               population <- data.frame(obj@population)
#               colnames(population) <- sprintf("Val_%s",seq(n_value))
#               return(invisible(population))
#           }
# )
# setMethod("getFitness", "nsga",
#           function(obj) {
#             algorithm <- toupper(class(obj)[1])
#             algorithm <- gsub('NSGA1', 'NSGA-I',
#                               gsub('NSGA2', 'NSGA-II',
#                                    gsub('NSGA3', 'NSGA-III',
#                                         algorithm)))
#             cat(algorithm, "Fitness: \n")
#             cat("\n#========================================#\n")
#             print(obj@fitness)
#             n_value <- ncol(obj@fitness)
#             fitness <- data.frame(obj@fitness)
#             colnames(fitness) <- sprintf("Fit_%s",seq(n_value))
#             return(invisible(fitness))
#           }
# )
# setGeneric("print", function(object) standardGeneric("print"))

#setGeneric("summary")

# El metodo plot es distinto, ya que indistintamente de que objeto estemos usuando
# plot siempre realizara la misma operacion con ella, i.e, pasando como argumento
# del metodo un objeto NSGA-I, NSGA-II, o NSGA-III y tipo de plot Scatter nos
# a graficar un scatter
.get.plotting <- function(x, y="missing",
                          type="scatter", ...){
  #if(is.null(type)) {
  #    stop( "x is not a numeric dataframe or matrix.")
  #}
  switch(type,
         "scatter" = {
           rmoo::scatter(x, ...)
         },
         "pcp"  = {
           rmoo::pcp(x, ...)
         },
         "heatmap" = {
           rmoo::heat_map(x, ...)
         },
         "polar" = {
           rmoo::polar(x, ...)
         }
  )
  #if(all(x@od > 1.E-06))
  # pca.ddplot(x, id.n.sd, id.n.od, ...)
  #else
  # pca.distplot(x, id.n.sd, ...)
}

setMethod("plot", signature(x="nsga1", y="missing"), .get.plotting)
setMethod("plot", signature(x="nsga2", y="missing"), .get.plotting)
setMethod("plot", signature(x="nsga3", y="missing"), .get.plotting)



setMethod("plot", signature(x="nsga3", y="missing"),
          function(x, y="missing",
                   type="scatter", ...){
  #if(is.null(type)) {
  #    stop( "x is not a numeric dataframe or matrix.")
  #}
  switch(type,
         "scatter" = {
           rmoo::scatter(x, ...)
         },
         "pcp"  = {
           rmoo::pcp(x, ...)
         },
         "heatmap" = {
           rmoo::heat_map(x, ...)
         },
         "polar" = {
           rmoo::polar(x, ...)
         }
  )
  #if(all(x@od > 1.E-06))
  # pca.ddplot(x, id.n.sd, id.n.od, ...)
  #else
  # pca.distplot(x, id.n.sd, ...)
}
)
# -----------------------------------------------------------------------------

# NO NECESARIA YA QUE LA FUNCIÓN RMOO VA A LLAMAR A LAS MISMAS FUNCIONES DE
# LOS ALGORITMOS.
# # Helper to choise what instances of nsga class create
# object.definition <- function(args = NULL){
#
#   #VALIDAR QUE EXISTE ESTOS ALGORITMOS ANTES DE CREAR LOS OBJETOS
#   if ("NSGA-I" %in% args$algorithm){
#     nsga <- new("nsga",
#                 call = args$call,
#                 type = args$type,
#                 lower = args$lower,
#                 upper = args$upper,
#                 nBits = args$nBits,
#                 names = if (is.null(args$names))
#                   character()
#                 else args$names,
#                 popSize = args$popSize,
#                 front = matrix(),
#                 f = list(),
#                 iter = 0,
#                 run = 1,
#                 maxiter = args$maxiter,
#                 suggestions = args$suggestions,
#                 population = matrix(),
#                 pcrossover = args$pcrossover,
#                 pmutation = if (is.numeric(args$pmutation))
#                   args$pmutation
#                 else NA,
#                 dumFitness = args$dum_Fitness,
#                 dShare = args$dshare,
#                 deltaDummy = args$delta_dum,
#                 fitness = args$Fitness,
#                 summary = args$fitnessSummary)
#   }
#   if ("NSGA-II" %in% args$algorithm){
#     out <- new("nsga2",
#                  call = call,
#                  type = type,
#                  lower = lower,
#                  upper = upper,
#                  nBits = nBits,
#                  names = if (is.null(names))
#                    character()
#                  else names,
#                  popSize = popSize,
#                  front = matrix(),
#                  f = list(),
#                  iter = 0,
#                  run = 1,
#                  maxiter = maxiter,
#                  suggestions = suggestions,
#                  population = matrix(),
#                  pcrossover = pcrossover,
#                  pmutation = if (is.numeric(pmutation))
#                    pmutation
#                  else NA,
#                  crowdingDistance = matrix(),
#                  fitness = Fitness,
#                  summary = fitnessSummary)
#   }
#   if ("NSGA-III" %in% args$algorithm){
#     nsga3 <- new("nsga3",
#                  call = argscall,
#                  type = type,
#                  lower = lower,
#                  upper = upper,
#                  nBits = nBits,
#                  names = if (is.null(names))
#                    character()
#                  else names,
#                  popSize = popSize,
#                  front = matrix(),
#                  f = list(),
#                  iter = 0,
#                  run = 1,
#                  maxiter = maxiter,
#                  suggestions = suggestions,
#                  population = matrix(),
#                  ideal_point = NA, #Agregar en nsga3-class
#                  worst_point = NA, #Agregar en nsga3-class
#                  smin = rep(NA, nObj),
#                  extreme_points = matrix(), #Agregar en nsga3-class
#                  worst_of_population = rep(NA, nObj), #Agregar en nsga3-class
#                  worst_of_front = rep(NA, nObj), #Agregar en nsga3-class
#                  nadir_point = rep(NA, nObj),
#                  pcrossover = pcrossover,
#                  pmutation = if (is.numeric(pmutation))
#                    pmutation
#                  else NA,
#                  reference_points = ref_dirs, #Agregar en nsga3-class
#                  fitness = Fitness,
#                  summary = fitnessSummary)
#   }
#
#  return(out)
# }
#
# object.definition <- function(args = callArgs)


# DEFINICIÓN DE METODOS
# Definimos las funciones genericas para especificar que nuestro paquete usara
# nombres de funciones que existen en el paquete 'base'

setMethod("getPopulation", "nsga",
          function(obj) {
            algorithm <- class(obj)[1]
            switch(algorithm,
                   "nsga1" = {
                     cat("NSGA-I Population: \n")
                     cat("\n#========================================#\n")

                     print(obj@population)
                   },
                   "nsga2"  = {
                     cat("NSGA-II Population: \n")
                     cat("\n#========================================#\n")
                     print(obj@population)
                   },
                   "nsga3" = {
                     cat("NSGA-III Population: \n")
                     cat("\n#========================================#\n")
                     print(obj@population)
                   }
            )
          }
)
setMethod("getFitness", "nsga",
          function(obj) {
            algorithm <- class(obj)[1]
            switch(algorithm,
                   "nsga1" = {
                     cat("NSGA-I Fitness: \n")
                     cat("\n#========================================#\n")

                     print(obj@fitness)
                   },
                   "nsga2"  = {
                     cat("NSGA-II Fitness: \n")
                     cat("\n#========================================#\n")
                     print(obj@fitness)
                   },
                   "nsga3" = {
                     cat("NSGA-III Fitness: \n")
                     cat("\n#========================================#\n")
                     print(obj@fitness)
                   }
            )
          }
)
setGeneric("plot")
#setGeneric("plot", function(x, y, type=NULL, ...) standardGeneric("psi"))

setGeneric("show")

setGeneric("print")
setMethod("print", "nsga", function(x, ...) str(x))
setMethod("print", "nsga1", function(x, ...) str(x))
setMethod("print", "nsga2", function(x, ...) str(x))
setMethod("print", "nsga3", function(x, ...) str(x))


setGeneric("summary")
setMethod("summary", "algorithm", function(object) invisible(object))
setMethod("summary", "nsga", function(object) invisible(object))
setMethod("summary", "nsga", summary.nsga)

summary.nsga <- function(object, ...) {




}
#Summary for NSGA Algorithm
setMethod("summary", "nsga1", function(object, ...) {

  callArgs <- list(...)
  nullRP <- is.null(callArgs$reference_dirs)
  # Calculate information for summary
  if (!nullRF) gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
  if (!nullRF) igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  first_dum <- object@dumFitness[first, ]
  if("emoa" %in% rownames(utils::installed.packages())){
    if(nullRP) {
      cat("Warning: reference point not provided:\n
          using the maximum in each dimension instead.")
      ref.point <- nadir.est
    }
    hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_dirs)
  }
  cat("\nSummary of MOEA/D run")
  cat("\n#====================================")
  cat("\nTotal function evaluations: ", ncol(object@fitness))
  cat("\nTotal iterations: ", object@iter)
  cat("\nPopulation size: ", object@popSIZE)
  cat("\nFeasible points found: ", nfeas,
      paste0("(", signif(100 * nfeas / npts, 3), "%"),
      "of total)")
  cat("\nNondominated points found: ", first,
      paste0("(", signif(100 * first / object@popSize, 3), "%"),
      "of total)")
  cat("\nEstimated ideal point: ", round(object@ideapoint, ndigits))
  cat("\nEstimated nadir point: ", round(object@nadirpoint, ndigits))
  if(!nullRF) cat("\nEstimated IGD: ", igd)
  if("emoa" %in% rownames(utils::installed.packages())) {
    cat("\nEstimated HV: ", hv)
    cat("\nRef point used for HV: ", ref.point)
  } else cat("\n\nPlease install package 'emoa' to calculate hypervolume.")
  cat("\n#====================================")
  #result <- list(`First Front Fit` = first_front_fit,
  #               `First Front Pop` = first_front_pop,
  #               `Dummy Front Fit` = first_dum,
  #               Hypervolumen = hv)
  return(result)
})

#Summary for NSGA-II Algorithm
setMethod("summary", "nsga2", function(object, ...) {
  # Calculate information for summary
  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  first_cd <- object@crowdingDistance[first, ]

  if("emoa" %in% rownames(utils::installed.packages())){
    if(nullRP) {
      cat("Warning: reference point not provided:\n
          using the maximum in each dimension instead.")
      ref.point <- nadir.est
    }
    hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = ref.point)
  }
  result <- list(`First Front Fit` = first_front_fit,
                 `First Front Pop` = first_front_pop,
                 `Crowding Dist` = first_cd,
                 Hypervolumen = hv)
  return(result)
})

#Summary for NSGA-III Algorithm
setMethod("summary", "nsga3", function(object, ...) {
  # Calculate information for summary
  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  ideal_point <- object@ideal_point
  worst_point <- object@worst_point
  extreme_points <- object@extreme_points
  gd <- ecr::computeGenerationalDistance(t(object@fitness), t(object@reference_points))
  igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(object@reference_points))
  if("emoa" %in% rownames(utils::installed.packages())){
    if(nullRP) {
      cat("Warning: reference point not provided:\n
          using the maximum in each dimension instead.")
      ref.point <- nadir.est
    }
    hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = ref.point)
  }
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
  return(result)
})

setMethod("show", "nsga1",
          function(object)
          { cat("An object of class \"nsga1\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })
setMethod("show", "nsga2",
          function(object)
          { cat("An object of class \"nsga2\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })
setMethod("show", "nsga3",
          function(object)
          { cat("An object of class \"nsga2\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })

setMethod("show", "nsga", function(object){
  cat("\nCall:\n")
  print(object@call)
  cat("-> Method: ", object@method, "\n")
  if(is.list(object@singularity))
    cat(strwrap(.MCDsingularityMsg(object@singularity, object@n.obs)), sep ="\n")

  digits = max(3, getOption("digits") - 3)
  cat("\nRobust Estimate of Location: \n")
  print.default(format(getCenter(object), digits = digits), print.gap = 2, quote = FALSE)
  cat("\nRobust Estimate of Covariance: \n")
  print.default(format(getCov(object), digits = digits), print.gap = 2, quote = FALSE)
  invisible(object)
})

#Creamos los metodos padres a modo de formalizacion, estos metodos no son utilizados
# ya que la ejecución del algoritmos nunca genera una instancia de algorithm

setMethod("plot", "algorithm", function(object) invisible(object))
setMethod("show", "algorithm", function(object) invisible(object))
setMethod("print", "algorithm", function(object) invisible(object))

#Creamos los metodos padres a modo de formalizacion, estos metodos no son utilizados
# ya que la ejecución del algoritmos nunca genera una instancia de nsga,
# de igual forma los metodos retorna el objecto de forma invisible, ya que al cargar
# el paquete el usuario puede instanciar una class de tipo nsga, y si desea utilizar algunos
# de estos metodos no obtendra retorno

setMethod("show", "nsga", function(object) invisible(object))
setMethod("plot", "nsga", function(object) invisible(object))
setMethod("print", "nsga", function(object) invisible(object))




# invisible(object)



  )

#getPopulation(object)
#getFitness(object)
#print()
#summary();
#show()
#plot()

#setMethod("print", "nsga1", function(x, ...) str(x))

#CREACIÓN DE METODOS USABLES.
setMethod("show", "nsga1",
          function(object)
          { cat("An object of class \"NSGA-I\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })

setMethod("show", "nsga2",
          function(object)
          { cat("An object of class \"NSGA-II\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })

setMethod("show", "nsga3",
          function(object)
          { cat("An object of class \"NSGA-III\"\n")
            cat("\nCall:\n", deparse(object@call), "\n\n",sep="")
            cat("Available slots:\n")
            print(slotNames(object))
          })



# Ejemplo de como hacer un metodo de plot, paquete rrcov

# setMethod("plot", signature(x="CovRobust", y="missing"),
#           function(x, y="missing",
#                    which=c("dd", "all", "distance", "qqchi2",
#                            "tolEllipsePlot", "pairs", "screeplot", "xydistance", "xyqqchi2"),
#                    classic= FALSE,
#                    ask = (which=="all" && dev.interactive(TRUE)),
#                    cutoff,
#                    id.n,
#                    labels.id = rownames(x$X),
#                    tol = 1e-7, ...) {
#             ## distance-distance plot: here we need both robust and mahalanobis distances
#             if((which == "all" || which == "dd") && !is.null(md) && !is.null(rd)) {
#               .myddplot(md, rd, cutoff=cutoff, id.n=id.n, ...) # distance-distance plot
#             }
#           }
#.myddplot <- function(md, rd, cutoff, id.n,
#                                 main="Distance-Distance Plot",
#                                 xlab="Mahalanobis distance",
#                                 ylab="Robust distance",
#                                 labs=1:length(md),
#                                 ...)
#{
#  Distance-Distance Plot:
#  Plot the vector y=rd (robust distances) against
#  x=md (mahalanobis distances). Identify by a label the id.n
#  observations with largest rd. If id.n is not supplied, calculate
#  it as the number of observations larger than cutoff. Use cutoff
#  to draw a horisontal and a vertical line. Draw also a dotted line
#  with a slope 1.
#             n <- length(md)
#             if(missing(id.n))
#               id.n <- length(which(rd>cutoff))
#
#             plot(md, rd, xlab=xlab, ylab=ylab, type="p", ...)
#             .label(md,rd,id.n, labs=labs)
#             abline(0, 1, lty=2)
#             abline(v=cutoff)
#             abline(h=cutoff)
#
#             title(main=main)
# }
# .label <- function(x, y, id.n=3, labs=1:length(x)) {
#   if(id.n > 0) {
#     xrange <- par("usr")
#     xrange <- xrange[2] - xrange[1]
#     n <- length(y)
#     ind <- sort(y, index.return=TRUE)$ix
#     ind <- ind[(n-id.n+1):n]
#     text(x[ind] + xrange/50, y[ind], labs[ind])
#   }
# }


#--------------------------------------------

validTrackObject <- function(object)
  if(length(x) == length(y))
    TRUE
else
  paste("Lengths of x (", length(x),
        ") and y (", length(y),
        ") should have been equal", sep="")

setClass("track",
         slots = list(x="numeric", y="numeric"),
         validity = validTrackObject)


var.names <- names(as.list(environment()))
var.names <- var.names[(length(var.names) - 1):1]

if(name %in% var.names){
  out <- get(name)
  return(out)
} else stop("Algorithm argument is not defined.")
}

stop_maxeval <- function(stopcrit, nfe, ...){
  maxeval.i <- which(sapply(stopcrit,
                            function(x) x$name) == "maxeval")
  maxeval   <- stopcrit[[maxeval.i]]$maxeval

  return(nfe >= maxeval)
}






#CRITERIOS DE STOP TOMADOS DE MOEADR

stopcrit = list(list(name    = "maxiter", maxiter = 50))

keep.running <- check_stop_criteria(stopcrit = stopcrit,
                                    call.env = environment())

check_stop_criteria <- function(stopcrit, call.env){


  crits <- unlist(lapply(stopcrit, function(x){x$name}))

  keep.running <- TRUE

  # Check criteria
  for (i in seq_along(crits)){
    function_name <- paste0("stop_", tolower(crits[i]))
    keep.running  <- keep.running  & !do.call(function_name,
                                              args = as.list(call.env))
  }

  # Output
  return(keep.running)
}



stop_maxtime <- function(stopcrit, iter.times, ...){

  t.pars <- stopcrit[[which(sapply(stopcrit, function(x)x$name) == "maxtime")]]

  elapsed.time       <- sum(iter.times)
  mean.iter.time     <- mean(iter.times)

  # return TRUE if there is not enough time remaining for another iteration to
  # be performed
  return(elapsed.time + mean.iter.time >= t.pars$maxtime)
}

stop_maxiter <- function(stopcrit, iter, ...){
  maxiter.i <- which(sapply(stopcrit, function(x) x$name) == "maxiter")
  maxiter   <- stopcrit[[maxiter.i]]$maxiter

  return(iter >= maxiter)
}

#Add GA package to import. It will be posible use s4 method from GA in rmoo.
#' @import GA


sphere     <- function(x){sum((x + seq_along(x) * 0.1) ^ 2)}
rastringin <- function(x){
         x.shift <- x - seq_along(x) * 0.1
         sum((x.shift) ^ 2 - 10 * cos(2 * pi * x.shift) + 10)}
problem.sr <- function(X){
              t(apply(X, MARGIN = 1,
               FUN = function(X){c(sphere(X), rastringin(X))}))}

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

## Run MOEA/D
out_moead <- moead(preset = NULL,
              problem, decomp, aggfun, neighbors, variation, update,
              constraint, scaling, stopcrit, showpars, seed)

## Examine the output:
summary(out_moead)
plot(out_moead)
print(out_moead)

out_moead$X #individuals
out_moead$Y #Fitness
out_moead$W #Reference points
