#' @export
nsgaMonitor <- function(object, number_objectives, ...) {
  if (!requireNamespace("rgl", quietly = TRUE)){
    stop("packages 'rgl' required for Monitor, please install it!")
  }
  # if(!all(requireNamespace("rgl", quietly = TRUE),
  #         requireNamespace("grDevices", quietly = TRUE))){
  #   stop("packages 'rgl' and 'grDevices' required for Monitor, please install it!")
  # }
  fitness <- object@fitness
  iter <- object@iter
  cl <- grDevices::rainbow(object@popSize)

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


.nsga1.progress <- function(object, ...) {
  callArgs <- as.list(...)

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
    result <- list(First_Front_Fit = first_front_fit,
                   First_Front_Pop = first_front_pop,
                   Dummy_Front_Fit = first_dum)

  } else{
    metric <- data.frame(Iternation = object@iter,
                         Generational_Distance = gd,
                         Inverse_Generational_Distance = igd,
                         Hypervolumen = hv)
    result <- list(First_Front_Fit = first_front_fit,
                   First_Front_Pop = first_front_pop,
                   Dummy_Front_Fit = first_dum,
                   metrics = metric)

  }
  return(invisible(result))
}

.nsga2.progress <- function(object, ...) {
  callArgs <- as.list(...)

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
    result <- list(First_Front_Fit = first_front_fit,
                   First_Front_Pop = first_front_pop,
                   Crowding_Dist = first_cd)

  } else{
    metric <- data.frame(Iternation = object@iter,
                         Generational_Distance = gd,
                         Inverse_Generational_Distance = igd,
                         Hypervolumen = hv)
    result <- list(First_Front_Fit = first_front_fit,
                   First_Front_Pop = first_front_pop,
                   Crowding_Dist = first_cd,
                   metrics = metric)

  }

  return(invisible(result))
}


.nsga3.progress <- function(object, ...) {
  callArgs <- as.list(...)
  reference_dirs <- callArgs$reference_dirs

  first <- object@f[[1]]
  first_front_fit <- object@fitness[first, ]
  first_front_pop <- object@population[first, ]
  ideal_point <- object@ideal_point
  worst_point <- object@worst_point
  extreme_points <- object@extreme_points

  if("ecr" %in% rownames(utils::installed.packages())){
    gd <- ecr::computeGenerationalDistance(t(object@fitness), t(reference_dirs))
    igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(reference_dirs))
  }
  if("emoa" %in% rownames(utils::installed.packages())){
    hv <- ecr::computeHV(t(object@fitness), ref.point = apply(reference_dirs, 2, max))
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

scatter <- function(object, ...){
  if(!all(requireNamespace("reshape2", quietly = TRUE),
          #requireNamespace("ggplot2", quietly = TRUE),
          #requireNamespace("plotly", quietly = TRUE),
          #requireNamespace("grDevices", quietly = TRUE),
          requireNamespace("cdata", quietly = TRUE))){
    stop("packages 'reshape2' and 'cdata' required for scatter plotting!")
  }
  #algorithm <- class(object)[1]
  n_obj <- ncol(object@fitness)
  if (n_obj == 2){
    plotting_multi_objective(object, ...)
  } else if (n_obj == 3) {
    plotting_many_objective(object, ...)
  } else{
    plotting_pairwise(object,...)

  }
}

plotting_multi_objective <- function(object, ...) {
  callArgs <- list(...)
  algorithm <- class(object)[1]
  if (any("optimal" %in% names(callArgs))) {
    optimal <- callArgs$optimal
    name_optimal <- lapply(substitute(list(...))[-1], deparse)$optimal
    objective_values <- object@fitness
    colnames(objective_values) <- c("f_1", "f_2")
    colnames(optimal) <- c("f_1", "f_2")
    optimal <- as.data.frame(optimal)
    objective_values <- as.data.frame(objective_values)

    ggplot2::ggplot() + geom_line(data = optimal,
                                  aes(x = f_1,
                                      y = f_2,
                                      color = name_optimal)) +
      ggplot2::geom_point(data = objective_values,
                          aes(x = f_1,
                              y = f_2,
                              color = "Objective_Value")) +
      ggplot2::labs(title = paste(algorithm, "No Objective:", ncol(object@fitness)),
                    color = "Values") +
      ggplot2::scale_color_manual(labels = c(name_optimal, "Objective_Value"),
                                  values = c("green", "black"))

  } else {
    objective_values <- object@fitness
    colnames(objective_values) <- c("f_1", "f_2")
    objective_values <- as.data.frame(objective_values)

    ggplot2::ggplot() + ggplot2::geom_point(data = objective_values,
                                            aes(x = f_1,
                                                y = f_2,
                                                color = "Objective_Values")) +
      ggplot2::labs(title = paste(algorithm, "No Objective:", ncol(object@fitness)),
                    color = "Values") +
      ggplot2::scale_color_manual(labels = "Objective_Value",
                                  values = "black")
  }
}

plotting_many_objective <- function(object, ...) {
  callArgs <- list(...)
  algorithm <- class(object)[1]
  objective_values <- object@fitness
  if (any("optimal" %in% names(callArgs))) {
    optimal <- callArgs$optimal
    shapes <- c(rep(0, nrow(objective_values)), rep(1, nrow(optimal)))

    temp <- rbind(objective_values, optimal)
    temp <- cbind(temp,shapes)
    colnames(temp) <- c("f_1", "f_2", "f_3", "shapes")
    temp <- as.data.frame(temp)
    temp$shapes[which(temp$shapes == 0)] <- "circle"
    temp$shapes[which(temp$shapes == 1)] <- "square"
    temp$shapes <- as.factor(temp$shapes)

    fig <- plotly::plot_ly(temp, x = ~f_1,
                           y = ~f_2,
                           z = ~f_3,
                           color = ~shapes,
                           colors = c('#0C4B8E', '#BF382A'),
                           size = 1)
    fig <- fig %>% plotly::add_markers()
    fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'f_1'),
                                               yaxis = list(title = 'f_2'),
                                               zaxis = list(title = 'f_3')))
    fig
  } else {
    colnames(objective_values) <- c("f_1", "f_2", "f_3")
    objective_values <- as.data.frame(objective_values)

    fig <- plotly::plot_ly(objective_values, x = ~f_1,
                           y = ~f_2,
                           z = ~f_3,
                           colors = '#0C4B8E', size = 1)
    fig <- fig %>% plotly::add_markers() #New plotly add
    fig <- fig %>% plotly::layout(scene = list(xaxis = list(title = 'f_1'),
                                               yaxis = list(title = 'f_2'),
                                               zaxis = list(title = 'f_3')))
    fig
  }
}

plotting_pairwise <- function(object, ...){
  fit <- as.data.frame(object@fitness)
  nObj <- ncol(object@fitness)
  algorithm <- class(object)[1]
  colnames(fit) <- sprintf("Obj_%s",seq(nObj))
  meas_vars <- colnames(fit)

  controlTable <- data.frame(expand.grid(meas_vars, meas_vars,
                                         stringsAsFactors = FALSE))

  colnames(controlTable) <- c("x", "y")

  controlTable <- cbind(data.frame(pair_key = paste(controlTable[[1]],
                                                    controlTable[[2]]),
                                   stringsAsFactors = FALSE),
                        controlTable)

  fit_aug <- cdata::rowrecs_to_blocks(fit, controlTable)

  splt <- strsplit(fit_aug$pair_key, split = " ", fixed = TRUE)
  fit_aug$columns <- vapply(splt, function(si) si[[1]], character(1))
  fit_aug$rows <- vapply(splt, function(si) si[[2]], character(1))

  fit_aug$columns <- factor(as.character(fit_aug$columns), meas_vars)
  fit_aug$rows <- factor(as.character(fit_aug$rows), meas_vars)

  ggplot2::ggplot(fit_aug, aes(x=x, y=y)) +
    ggplot2::geom_point(aes(color=rows, shape=columns)) +
    ggplot2::facet_grid(rows~columns,
                        labeller = label_value,
                        scale = "free") +
    ggplot2::ggtitle(paste(algorithm, "No Objective:", nObj)) +
    ggplot2::scale_shape_manual(values=seq(nObj)) +
    ggplot2::scale_fill_manual(values = grDevices::rainbow(nObj)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90)) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL)
}

pcp <- function(object) {

  if (!requireNamespace("reshape2", quietly = TRUE)){
    stop("packages 'reshape2' required for pcp plotting!")
  }

  # if(!all(requireNamespace("ggplot2", quietly = TRUE),
  #         requireNamespace("grDevices", quietly = TRUE),
  #         requireNamespace("reshape2", quietly = TRUE),
  #         requireNamespace("dplyr", quietly = TRUE)))
  #   stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for pcp plotting!")
  nObj <- ncol(object@fitness)
  colnames(object@fitness) <- sprintf("f_%s",seq(nObj))
  fitness <- reshape2::melt(object@fitness)
  fitness$color <- grDevices::rainbow(object@popSize)
  fitness <- dplyr::rename(fitness,
                           'Objective_Value' = value,
                           'Objective_No' = Var2)
  ggplot2::ggplot(fitness, aes(x = Objective_No,
                               y = Objective_Value,
                               group = Var1, colour=factor(color))) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::theme_classic()
}

heat_map <- function(object, ...){
  if (!requireNamespace("reshape2", quietly = TRUE)){
    stop("packages 'reshape2' required for heat map plotting!")
  }
  # if(!all(requireNamespace("ggplot2", quietly = TRUE),
  #         requireNamespace("reshape2", quietly = TRUE),
  #         requireNamespace("dplyr", quietly = TRUE)))
  #   stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for heat map plotting!")
  callArgs <- list(...)
  individual <- callArgs$individual
  fitness <- object@fitness

  if(is.null(individual))
    stop("Please, define a vector with the individuals to plot")

  if (length(individual) > 10) {
    cat("Warning! Heatmap plot with more than 10 individuals will not be displayed correctly.\n
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
  if (!requireNamespace("reshape2", quietly = TRUE)){
    stop("packages 'reshape2' required for polar coordinate plotting!")
  }
  # if(!all(requireNamespace("ggplot2", quietly = TRUE),
  #         requireNamespace("reshape2", quietly = TRUE),
  #         requireNamespace("dplyr", quietly = TRUE)))
  #   stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for polar coordinate plotting!")

  callArgs <- list(...)
  individual <- callArgs$individual
  fitness <- object@fitness

  if(is.null(individual))
    stop("Please, define a vector with the individuals to plot")

  if (length(individual) > 10) {
    cat("Warning! Polar Coordinate plot with more than 10 individuals will not be displayed correctly.\n
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

utils::globalVariables(c("Pop","Objective_No", "Objective_Value", "Var1", "Var2", "color", "columns", "f_1", "f_2", "label_both", "rows", "value", "x", "y"))




# @export
# summary.rmoo <- function(object, ...) {
#   algorithm <- class(object)[1]
#   if(algorithm == "nsga1"){
#     out <- nsgaSummary(object)
#   }
#   else if(algorithm == "nsga2"){
#     out <- nsgaiiSummary(object)
#   }
#   else if(algorithm == "nsga3"){
#     out <- nsgaiiiSummary(object)
#   }
#   #class(out) <- "summary"
#   return(out)
# }

#colMax <- function(data) apply(data, 2, max)

#Summary for NSGA Algorithm
# nsgaSummary <- function(object, ...) {
#   # Calculate information for summary
#   callArgs <- list(...)
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
#   return(result)
# }
#
# #Summary for NSGA-II Algorithm
# nsgaiiSummary <- function(object, ...) {
#   # Calculate information for summary
#   callArgs <- list(...)
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
#   return(result)
# }
#
# #Summary for NSGA-III Algorithm
# nsgaiiiSummary <- function(object, ...) {
#   # Calculate information for summary
#   first <- object@f[[1]]
#   first_front_fit <- object@fitness[first, ]
#   first_front_pop <- object@population[first, ]
#   ideal_point <- object@ideal_point
#   worst_point <- object@worst_point
#   extreme_points <- object@extreme_points
#
#   if("ecr" %in% rownames(utils::installed.packages())){
#       gd <- ecr::computeGenerationalDistance(t(object@fitness), t(object@reference_points))
#       igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(object@reference_points))
#   }
#   if("emoa" %in% rownames(utils::installed.packages())){
#       hv <- ecr::computeHV(t(object@fitness), ref.point = apply(object@reference_points, 2, max))
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
#   return(result)
# }
