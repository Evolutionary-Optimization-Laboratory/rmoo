#' Accessor methods to the crowding distance for NSGA-II results
#'
#' @name getCrowdingDistance-method
#'
#' @param obj an object resulting from the execution of NSGA-II algorithm
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return Returns a vector with the crowding distances of class nsga2. See [nsga2-class]
#' for a description of available slots information.
#'
#' @examples
#' # Where 'out' is an object resulting from the execution of the NSGA-II algorithm.
#' #
#' # getCrowdingDistance(out)
#' #
#'
#' @export
setGeneric("getCrowdingDistance", function(obj) standardGeneric("getCrowdingDistance"))

#' Accessor methods to the dummy fitness for NSGA-I results
#'
#' @name getDummyFitness-method
#'
#' @param obj an object resulting from the execution of NSGA-I algorithm
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return Returns a matrix with the dummy fitness of class nsga1. See [nsga1-class]
#' for a description of available slots information.
#'
#' @examples
#' # Where 'out' is an object resulting from the execution of the NSGA-I algorithm.
#' #
#' # getDummyFitness(out)
#' #
#'
#' @export
setGeneric("getDummyFitness", function(obj) standardGeneric("getDummyFitness"))

#' Accessor methods to the population for rmoo results
#'
#' @name getPopulation-method
#'
#' @param obj an object resulting from the execution of NSGA-I, NSGA-II or NSGA-III
#' algorithm
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return Prints the resulting population and when the result of the method-call
#' is assigned to a variable, the population is stored as a data frame.
#' See [nsga1-class] [nsga2-class], [nsga3-class] for a description of available
#' slots information.
#'
#' @examples
#' # Where 'out' is an object resulting from the execution of rmoo.
#' #
#' # population_result <- getPopulation(out)
#' #
#' # population_result
#'
#' @export
setGeneric("getPopulation", function(obj) standardGeneric("getPopulation"))

#' Accessor methods to the fitness for rmoo results
#'
#' @name getFitness-method
#'
#' @param obj an object resulting from the execution of NSGA-I, NSGA-II or NSGA-III
#' algorithm
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return Prints the resulting fitness and when the result of the method-call
#' is assigned to a variable, the fitness is stored as a data frame.
#' See [nsga1-class] [nsga2-class], [nsga3-class] for a description of available
#' slots information.
#'
#' @examples
#' # Where 'out' is an object resulting from the execution of the rmoo.
#' #
#' # fitness_result <- getFitness(out)
#' #
#' # fitness_result
#'
#' @export
setGeneric("getFitness", function(obj) standardGeneric("getFitness"))


#' Methods for Function 'plot' in Package 'rmoo'
#'
#' Method used to visualize the fitness of the individuals during the execution
#' of the algorithms.
#'
#' The following plots are available:
#'
#' \itemize{
#'  \item{}{"Scatter Plot"}
#'  \item{}{"Parallel Coordinate Plot"}
#'  \item{}{"Heat Map"}
#'  \item{}{"Polar Coordinate"}
#' }
#'
#' @param x,y Objects of either class \linkS4class{nsga1},
#'   \linkS4class{nsga2},  or \linkS4class{nsga3}.
#' @param ... other arguments passed on to methods
#' \describe{
#' 	\item{"type"}{Type of graph to draw, the graphs can be of the type "scatter",
#' 	"pcp", "heatmap", or "polar"}
#'	\item{"optimal"}{An argument passed to the "scatter" plot. A matrix of
#'	dimension equal to the fitness with which they are compared. This value can
#'	only be compared in 2 and 3 dimensional "scatter" plots.}
#' 	\item{"individual"}{An argument passed to the "heatmap" and "polar" plots.
#' 	A vector that represents the fitness of the individuals to be displayed.}
#' }
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return A graph of the evaluated type.
#'
#' @examples
#' # Where 'out' is an object of class nsga1, nsga2, or nsga3.
#' # The plot method will by default plot a scatter plot.
#' #
#' # plot(out)
#' #
#' # The Parallel Coordinate Plot will be plotted if "pcp" is passed as a parameter to "type".
#' #
#' # plot(out, type="pcp")
#' #
#' # A heat map plot will be plotted if "heatmap" is passed as a parameter to "type"
#' # and a vector with the individuals to plot to "individual"
#' #
#' # plot(out, type = "heatmap", individual = c(1:5))
#' #
#' # A polar coordinate plot will be plotted if "polar" is passed as a parameter to "type"
#' # and a vector with the individuals to plot to "individual"
#' #
#' # plot(out, type = "polar", individual = c(1:5))
#'
#' @name plot-method
NULL

#' @rdname plot-method
#' @export
if (!isGeneric("plot"))
   setGeneric("plot", function(x, y, ...) standardGeneric("plot"))


#' Methods for Function 'print' in Package 'rmoo'.
#'
#' Method used to print the slots and relevant values of the object.
#'
#' @param x,y Objects of either class \linkS4class{nsga1},
#'   \linkS4class{nsga2},  or \linkS4class{nsga3}.
#' @param ... other arguments passed on to methods
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return Print the slots and relevant values of the object.
#'
#' @examples
#' # Where 'out' is an object of class nsga1, nsga2, or nsga3
#' #
#' # print(out)
#'
#' @name print-method
NULL

#' @rdname print-method
#' @export
if (!isGeneric("print"))
   setGeneric("print", function(x, y, ...) standardGeneric("print"))

#' Methods for Function 'summary' in Package 'rmoo'
#'
#' Method used to summarize the results of the evaluations, passing additional
#' arguments in the summary method the performance metrics is evaluated.
#'
#' @param x,y Objects of either class \linkS4class{nsga1},
#'   \linkS4class{nsga2},  or \linkS4class{nsga3}.
#' @param ... other arguments passed on to methods. Passing \code{"reference_dirs"}
#' as arguments will evaluate the performance metrics Hypervolumen,
#' Generational Distance, and Inverse Generational Distance.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return A summary of the values resulting from the execution of an algorithm.
#'
#' @examples
#' # Where 'out' is an object of class nsga1, nsga2, or nsga3
#' #
#' # summary(out)
#' #
#' # For the evaluation of the metrics, pass the reference point
#' #
#' # ref_points <- generate_reference_points(3,12)
#' # summary(out, reference_dirs = ref_points)
#'
#' @name summary-method
NULL

#' @rdname summary-method
#' @export
if (!isGeneric("summary"))
  setGeneric("summary", function(x, y, ...) standardGeneric("summary"))

#' Methods for Function 'progress' in Package 'rmoo'
#'
#' Method used to save the progress of the evaluation results, similar to the
#' summary method. Passing additional arguments to the progress method evaluates
#' performance metrics per iteration. This method cannot be called outside of
#' rmoo execution.
#'
#' @name progress-method
#'
#' @param x,y Objects of either class \linkS4class{nsga1},
#'   \linkS4class{nsga2},  or \linkS4class{nsga3}.
#' @param ... other arguments passed on to methods. Passing \code{"reference_dirs"}
#' as arguments will evaluate the performance metrics Hypervolumen,
#' Generational Distance, and Inverse Generational Distance.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return A list of length equal to the number of iterations, where the progress made during execution is saved.
#'
#' @examples
#' # Where 'out' is an object of class nsga1, nsga2, or nsga3, and callArgs are
#' # the additional arguments passed when calling the rmoo function, for the
#' # evaluation of performance metrics, reference points are expected to be passed
#' # as an argument to reference_dirs.
#' #
#' # progress(object, callArgs)
#' #
#'
#' @export
setGeneric("progress", function(object, ...) standardGeneric("progress"))

#' Accessor methods to the metrics evaluated during execution
#'
#' @name getMetrics-method
#'
#' @param obj an object resulting from the execution of NSGA-I, NSGA-II or NSGA-III
#' algorithm. During the execution of the performance metrics must be evaluated.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @return A dataframe with performance metrics evaluated iteration by iteration.
#'
#' @examples
#' # Where 'out' is an object resulting from the execution of the rmoo.
#' #
#' # metrics_result <- getMetrics(out)
#' #
#' # metrics_result
#'
#' @export
setGeneric("getMetrics", function(obj) standardGeneric("getMetrics"))
# if (!isGeneric("progress"))


#' @export
setMethod("getPopulation", "nsga",
          function(obj) {
            print(obj@population)
            n_value <- ncol(obj@population)
            population <- data.frame(obj@population)
            colnames(population) <- sprintf("Val_%s",seq(n_value))
            return(invisible(population))
          }
)

#' @export
setMethod("getFitness", "nsga",
          function(obj) {
            print(obj@fitness)
            n_value <- ncol(obj@fitness)
            fitness <- data.frame(obj@fitness)
            colnames(fitness) <- sprintf("Fit_%s",seq(n_value))
            return(invisible(fitness))
          }
)


#' @export
setMethod("print", "nsga",
          function(x=object, y="missing", ...) {
            # algorithm <- class(object)[1]
            # Print
            cat("Slots Configuration:\n")
            print((slotNames(x)))
            cat("\n#========================================#\n")
            cat("\nTotal iterations: ", x@iter)
            cat("\nPopulation size: ", x@popSize)
            if (x@type == "binary") {
              cat("\nNumber of Bits: ", x@nBits)
            } else{
              cat("\nLower Bounds: ", x@lower)
              cat("\nLower Bounds: ", x@upper)
            }
            cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
            cat("\n#========================================#\n")
          }
)

#' @export
setMethod("plot", signature(x="nsga", y="missing"), .get.plotting)

#' @export
setMethod("summary", "nsga",
          function(object, ...) {
            str(object)
          }
)


#' @export
setMethod("getMetrics", "nsga",
          function(obj) {
            iter <- obj@iter
            out <- data.frame()
            if(!is.null(obj@summary[[1]]$metrics)){
              for (i in seq_len(iter)){
                out <- rbind(out, obj@summary[[i]]$metrics)
              }
            } else {
              stop("No metrics have been evaluated.")
            }
            return(out)
          }
)


# -----------------------------------------------------------------------------
# setMethod("getCrowdingDistance", "nsga2", function(obj) print(obj@crowdingDistance))
# setMethod("getDummyFitness", "nsga1",
#           function(obj) {
#             cat("NSGA-I Dummy Fitness: \n")
#             cat("\n#========================================#\n")
#             print(obj@dumFitness)
#             n_dum <- ncol(obj@dumFitness)
#             dum_Fitness <- data.frame(obj@dumFitness)
#             colnames(dum_Fitness) <- sprintf("FitDummy_%s",seq(n_dum))
#             return(invisible(dum_Fitness))
#           }
# )

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# setMethod("print", "nsga1",
#           function(x=object, y="missing", ...) {
#             algorithm <- class(x)[1]
#             # Print
#             cat("\nSlots Configuration:\n")
#             print(as.list(slotNames(x)))
#             cat("\n#========================================#\n")
#             cat("\nTotal iterations: ", x@iter)
#             cat("\nPopulation size: ", x@popSize)
#             cat("\nLower Bounds: ", x@lower)
#             cat("\nLower Bounds:  ", x@upper)
#             cat("\nDelta Distance (dShare):  ", x@dShare)
#             cat("\nDistance of sharing function:  ", x@deltaDummy)
#             cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
#             cat("\n#========================================#\n")
#
#           }
# )

# setMethod("print", "nsga3",
#           function(x=object, y="missing", ...) {
#             # algorithm <- class(object)[1]
#             # Print
#             cat("Slots Configuration:\n")
#             print(as.list(slotNames(x)))
#             cat("\n#========================================#\n")
#             cat("\nTotal iterations: ", x@iter)
#             cat("\nPopulation size: ", x@popSize)
#             cat("\nLower Bounds: ", x@lower)
#             cat("\nUpper Bounds:  ", x@upper)
#             cat("\nEstimated Ideal Point:  ", x@ideal_point)
#             cat("\nEstimated Worst Point:  ", x@worst_point)
#             cat("\nEstimated Nadir Point:  ", x@nadir_point)
#             cat("\nNumber of Nondominated Front:  ", length(x@f[[1]]))
#             cat("\n#========================================#\n")
#           }
# )

# -----------------------------------------------------------------------------

# setMethod("summary", "nsga3",
#           function(object, ...){
#               callArgs <- list(...)
#               nullRP <- is.null(callArgs$reference_dirs)
#
#               # Calculate information for summary
#
#               first <- object@f[[1]]
#               first_front_fit <- object@fitness[first, ]
#               first_front_pop <- object@population[first, ]
#               nadir_point <- object@nadir_point
#
#               #first_dum <- object@dumFitness[first, ] for nsga1 summary method
#
#               if("ecr" %in% rownames(utils::installed.packages())){
#                 if (nullRP) {
#                   cat("Warning: reference points not provided:\n
#                       value necessary to evaluate GD and IGD.")
#
#                 } else{
#                   gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#                   igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#                 }
#               }
#
#               if("emoa" %in% rownames(utils::installed.packages())){
#                 if(nullRP) {
#                   cat("Warning: reference points not provided:\n
#                       using the maximum in each dimension to evaluate Hypervolumen")
#                   reference_point <- nadir_point
#                 } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
#                 hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
#               }
#
#               cat("\nSummary of NSGA-III run")
#               cat("\n#====================================")
#               cat("\nNumber of Objectives evaluated: ", ncol(object@fitness))
#               cat("\nTotal iterations: ", object@iter)
#               cat("\nPopulation size: ", object@popSize)
#               #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
#               cat("\nNondominated points found: ", length(first),
#                   paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
#                   "of total)")
#               cat("\nEstimated ideal point: ", round(object@ideal_point, 3))
#               cat("\nEstimated nadir point: ", round(object@nadir_point, 3))
#               cat("\nMutation Probability: ",
#                   paste0(signif(100 * object@pmutation, 3), "%"))
#               cat("\nCrossover Probability: ",
#                   paste0(signif(100 * object@pcrossover, 3), "%"))
#               if("ecr" %in% rownames(utils::installed.packages())){
#                 if(!nullRP) cat("\nEstimated IGD: ", igd)
#                 if(!nullRP) cat("\nEstimated GD: ", gd)
#               } else cat("\n\nPlease install package 'ecr' to calculate IGD and GD.")
#               if("emoa" %in% rownames(utils::installed.packages())) {
#                 cat("\nEstimated HV: ", hv)
#                 cat("\nRef point used for HV: ", reference_point)
#               } else cat("\n\nPlease install package 'emoa' to calculate hypervolume.")
#               cat("\n#====================================")
#           }
# )

# setMethod("summary", "nsga2",
#           function(object, ...){
#             callArgs <- list(...)
#             nullRP <- is.null(callArgs$reference_dirs)
#
#             # Calculate information for summary
#
#             first <- object@f[[1]]
#             first_front_fit <-
#             first_front_pop <- object@population[first, ]
#             nadir_point <- apply(object@fitness[first, ], 2, max)
#
#             #first_dum <- object@dumFitness[first, ] for nsga1 summary method
#
#             if("ecr" %in% rownames(utils::installed.packages())){
#               if (nullRP) {
#                 cat("Warning: reference points not provided:\n
#                       value necessary to evaluate GD and IGD.")
#
#               } else{
#                 gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#                 igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#               }
#             }
#
#             if("emoa" %in% rownames(utils::installed.packages())){
#               if(nullRP) {
#                 cat("Warning: reference points not provided:\n
#                       using the maximum in each dimension to evaluate Hypervolumen")
#                 reference_point <- nadir_point
#               } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
#               hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
#             }
#
#             cat("\nSummary of NSGA-II run")
#             cat("\n#====================================")
#             cat("\nNumber of Objectives evaluated: ", ncol(object@fitness))
#             cat("\nTotal iterations: ", object@iter)
#             cat("\nPopulation size: ", object@popSize)
#             #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
#             cat("\nNondominated points found: ", length(first),
#                 paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
#                 "of total)")
#             cat("\nCrowding distance bounds: ", c(max(object@crowdingDistance), min(object@crowdingDistance)))
#             #cat("\nEstimated nadir point: ", round(object@nadir_point, 3))
#             cat("\nMutation Probability: ",
#                 paste0(signif(100 * object@pmutation, 3), "%"))
#             cat("\nCrossover Probability: ",
#                 paste0(signif(100 * object@pcrossover, 3), "%"))
#             if("ecr" %in% rownames(utils::installed.packages())){
#               if(!nullRP) cat("\nEstimated IGD: ", igd)
#               if(!nullRP) cat("\nEstimated GD: ", gd)
#             } else cat("\n\nPlease install package 'ecr' to calculate IGD and GD.")
#             if("emoa" %in% rownames(utils::installed.packages())) {
#               cat("\nEstimated HV: ", hv)
#               cat("\nRef point used for HV: ", reference_point)
#             } else cat("\n\nPlease install package 'emoa' to calculate hypervolume.")
#             cat("\n#====================================")
#           }
# )

# setMethod("summary", "nsga1",
#           function(object, ...){
#             callArgs <- list(...)
#             nullRP <- is.null(callArgs$reference_dirs)
#
#             # Calculate information for summary
#
#             first <- object@f[[1]]
#             first_front_fit <-
#             first_front_pop <- object@population[first, ]
#             nadir_point <- apply(object@fitness[first, ], 2, max)
#
#             #first_dum <- object@dumFitness[first, ] for nsga1 summary method
#
#             if("ecr" %in% rownames(utils::installed.packages())){
#               if (nullRP) {
#                 cat("Warning: reference points not provided:\n
#                       value necessary to evaluate GD and IGD.")
#
#               } else{
#                 gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#                 igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#               }
#             }
#
#             if("emoa" %in% rownames(utils::installed.packages())){
#               if(nullRP) {
#                 cat("Warning: reference points not provided:\n
#                       using the maximum in each dimension to evaluate Hypervolumen")
#                 reference_point <- nadir_point
#               } else {reference_point <- apply(callArgs$reference_dirs, 2, max)}
#               hv <- emoa::dominated_hypervolume(points = t(object@fitness[first, ]), ref = reference_point)
#             }
#
#             cat("\nSummary of NSGA-I run")
#             cat("\n#====================================")
#             cat("\nNumber of Objectives evaluated: ", ncol(object@fitness))
#             cat("\nTotal iterations: ", object@iter)
#             cat("\nPopulation size: ", object@popSize)
#             #cat("\nFeasible points found: ", nfeas,paste0("(", signif(100 * nfeas / npts, 3), "%"),"of total)")
#             cat("\nNondominated points found: ", length(first),
#                 paste0("(", signif(100 * length(first) / object@popSize, 3), "%"),
#                 "of total)")
#             cat("\nShare Distance: ", object@dShare)
#             cat("\nSharing Values calculated: ", object@deltaDummy)
#             #cat("\nEstimated nadir point: ", round(object@nadir_point, 3))
#             cat("\nMutation Probability: ",
#                 paste0(signif(100 * object@pmutation, 3), "%"))
#             cat("\nCrossover Probability: ",
#                 paste0(signif(100 * object@pcrossover, 3), "%"))
#             if("ecr" %in% rownames(utils::installed.packages())){
#               if(!nullRP) cat("\nEstimated IGD: ", igd)
#               if(!nullRP) cat("\nEstimated GD: ", gd)
#             } else cat("\n\nPlease install package 'ecr' to calculate IGD and GD.")
#             if("emoa" %in% rownames(utils::installed.packages())) {
#               cat("\nEstimated HV: ", hv)
#               cat("\nRef point used for HV: ", reference_point)
#             } else cat("\n\nPlease install package 'emoa' to calculate hypervolume.")
#             cat("\n#====================================")
#           }
# )

# -----------------------------------------------------------------------------

# heat_map <- function(object, ...){
#   if(!all(requireNamespace("ggplot2", quietly = TRUE),
#           requireNamespace("reshape2", quietly = TRUE),
#           requireNamespace("dplyr", quietly = TRUE)))
#     stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for heat map plotting!")
#   callArgs <- list(...)
#   individual <- callArgs$individual
#   fitness <- object@fitness
#
#   if(is.null(individual))
#     stop("Please, define a vector with the individuals to plot")
#
#   if (length(individual) > 10) {
#     cat("Warning: Heatmap plot with more than 10 individuals will not be displayed correctly.\n
#                   The plot will still be displayed.")
#   }
#
#   fitness <- fitness[individual,]
#
#   if (is.null(dim(fitness))){
#     fitness <- t(matrix(fitness))
#   }
#
#   nObj <- ncol(fitness)
#   colnames(fitness) <- sprintf("f_%s",seq(nObj))
#   fitness <- reshape2::melt(fitness)
#   fitness <- dplyr::rename(fitness,
#                            'Pop' = Var1,
#                            'Objective_Value' = value,
#                            'Objective_No' = Var2)
#   ggplot2::ggplot(fitness, aes(x = Objective_No,
#                                y = Pop,
#                                fill = Objective_Value)) +
#     ggplot2::geom_raster() +
#     ggplot2::scale_y_continuous(labels = unique(as.character(fitness$Pop)),
#                                 breaks = unique(fitness$Pop))
# }

# polar <- function(object, ...){
#   if(!all(requireNamespace("ggplot2", quietly = TRUE),
#           requireNamespace("reshape2", quietly = TRUE),
#           requireNamespace("dplyr", quietly = TRUE)))
#     stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for polar coordinate plotting!")
#
#   callArgs <- list(...)
#   individual <- callArgs$individual
#   fitness <- object@fitness
#
#   if(is.null(individual))
#     stop("Please, define a vector with the individuals to plot")
#
#   if (length(individual) > 10) {
#     cat("Warning: Polar Coordinate plot with more than 10 individuals will not be displayed correctly.\n
#                   The plot will still be displayed.")
#   }
#
#   fitness <- fitness[individual,]
#
#   if (is.null(dim(fitness))){
#     fitness <- t(matrix(fitness))
#   }
#
#   nObj <- ncol(fitness)
#   colnames(fitness) <- sprintf("f_%s",seq(nObj))
#   fitness <- reshape2::melt(fitness)
#   fitness <- dplyr::rename(fitness,
#                            'Pop' = Var1,
#                            'Objective_Value' = value,
#                            'Objective_No' = Var2)
#   ggplot2::ggplot(fitness, aes(x = Objective_No,
#                                y = Objective_Value,
#                                fill = Objective_No)) +
#     ggplot2::geom_bar(width = 1, stat="identity") +
#     ggplot2::coord_polar() + ggplot2::theme_light() +
#     ggplot2::facet_wrap(~Pop, nrow = 1)
# }



# .get.plotting <- function(x, y="missing",
#                           type="scatter", ...){
#   switch(type,
#          "scatter" = {
#            scatter(x, ...)
#          },
#          "pcp"  = {
#            pcp(x, ...)
#          },
#          "heatmap" = {
#            heat_map(x, ...)
#          },
#          "polar" = {
#            polar(x, ...)
#          }
#   )
# }

# setMethod("plot", signature(x="nsga1", y="missing"), .get.plotting)
# setMethod("plot", signature(x="nsga2", y="missing"), .get.plotting)
# setMethod("plot", signature(x="nsga3", y="missing"), .get.plotting)

# -----------------------------------------------------------------------------

# .nsga1.progress <- function(object, ...) {
#   callArgs <- as.list(...)
#
#   nullRP <- is.null(callArgs$reference_dirs)
#
#   first <- object@f[[1]]
#   first_front_fit <- object@fitness[first, ]
#   first_front_pop <- object@population[first, ]
#   first_dum <- object@dumFitness[first, ]
#
#   if("ecr" %in% rownames(utils::installed.packages())){
#     if (!nullRP) {
#       gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#       igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#     }
#   }
#
#   if("emoa" %in% rownames(utils::installed.packages())){
#     if(!nullRP) {
#       reference_point <- apply(callArgs$reference_dirs, 2, max)
#       hv <- emoa::dominated_hypervolume(points = t(first_front_fit), ref = reference_point)
#     }
#   }
#
#   if(nullRP) {
#     result <- list(`First Front Fit` = first_front_fit,
#                    `First Front Pop` = first_front_pop,
#                    `Dummy Front Fit` = first_dum)
#
#   } else{
#     result <- list(`First Front Fit` = first_front_fit,
#                    `First Front Pop` = first_front_pop,
#                    `Dummy Front Fit` = first_dum,
#                    Hypervolumen = hv,
#                    `Generational Distance` = gd,
#                    `InvertedGenerational Distance` = igd)
#
#   }
#   return(invisible(result))
# }
#
# .nsga2.progress <- function(object, ...) {
#   callArgs <- as.list(...)
#
#   nullRP <- is.null(callArgs$reference_dirs)
#
#   first <- object@f[[1]]
#   first_front_fit <- object@fitness[first, ]
#   first_front_pop <- object@population[first, ]
#   first_cd <- object@crowdingDistance[first, ]
#
#   if("ecr" %in% rownames(utils::installed.packages())){
#     if (!nullRP) {
#       gd <- ecr::computeGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#       igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(callArgs$reference_dirs))
#     }
#   }
#   if("emoa" %in% rownames(utils::installed.packages())){
#     if(!nullRP) {
#       reference_point <- apply(callArgs$reference_dirs, 2, max)
#       hv <- emoa::dominated_hypervolume(points = t(first_front_fit), ref = reference_point)
#     }
#   }
#
#   if(nullRP) {
#     result <- list(`First Front Fit` = first_front_fit,
#                    `First Front Pop` = first_front_pop,
#                    `Crowding Dist` = first_cd)
#
#   } else{
#     result <- list(`First Front Fit` = first_front_fit,
#                    `First Front Pop` = first_front_pop,
#                    `Crowding Dist` = first_cd,
#                    Hypervolumen = hv,
#                    `Generational Distance` = gd,
#                    `InvertedGenerational Distance` = igd)
#
#   }
#
#   return(invisible(result))
# }
#
#
# .nsga3.progress <- function(object, ...) {
#   first <- object@f[[1]]
#   first_front_fit <- object@fitness[first, ]
#   first_front_pop <- object@population[first, ]
#   ideal_point <- object@ideal_point
#   worst_point <- object@worst_point
#   extreme_points <- object@extreme_points
#
#   if("ecr" %in% rownames(utils::installed.packages())){
#     gd <- ecr::computeGenerationalDistance(t(object@fitness), t(object@reference_points))
#     igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(object@reference_points))
#   }
#   if("emoa" %in% rownames(utils::installed.packages())){
#     hv <- ecr::computeHV(t(object@fitness), ref.point = apply(object@reference_points, 2, max))
#   }
#   if (all((c("ecr", "emoa") %in% rownames(utils::installed.packages())))) {
#     metric <- data.frame(Iternation = object@iter,
#                          Generational_Distance = gd,
#                          Inverse_Generational_Distance = igd,
#                          Hypervolumen = hv)
#     result <- list(first_front_fit = first_front_fit,
#                    first_front_pop = first_front_pop,
#                    ideal_point = ideal_point,
#                    worst_point = worst_point,
#                    extreme_points = extreme_points,
#                    metrics = metric)
#   } else{
#     result <- list(first_front_fit = first_front_fit,
#                    first_front_pop = first_front_pop,
#                    ideal_point = ideal_point,
#                    worst_point = worst_point,
#                    extreme_points = extreme_points)
#   }
#   return(invisible(result))
# }


#setMethod("progress", "nsga1", .nsga1.progress)
#setMethod("progress", "nsga2", .nsga2.progress)
#setMethod("progress", "nsga3", .nsga3.progress)


