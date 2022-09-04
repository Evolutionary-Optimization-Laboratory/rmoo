#' Scatter Plot Functions
#'
#' Allows to make scatter plots in publication quality allowing
#' to represent 2-D, 3-D and M-D
#'
#' @param object An object of nsga-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
#' @param ... Other arguments passed on to methods. Used to pass the `optimal`
#' value of the objective function, in case of having it.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @examples
#' #Two Objectives Plotting
#' zdt1 <- function (x) {
#'  if (is.null(dim(x))) {
#'    x <- matrix(x, nrow = 1)
#'  }
#'  n <- ncol(x)
#'  g <- 1 + rowSums(x[, 2:n, drop = FALSE]) * 9/(n - 1)
#'  return(cbind(x[, 1], g * (1 - sqrt(x[, 1]/g))))
#' }
#'
#' #Not run
#' \dontrun{
#' result <- nsga3(type = "real-valued",
#'                 fitness = zdt1,
#'                 lower = c(0,0),
#'                 upper = c(1,1),
#'                 popSize = 100,
#'                 n_partitions = 100,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#' #Not run
#' \dontrun{
#' scatter(object = result)
#' }
#'
#' @export
scatter <- function(object, ...){
  if(!all(requireNamespace("ggplot2", quietly = TRUE),
    requireNamespace("reshape2", quietly = TRUE),
    requireNamespace("plotly", quietly = TRUE),
    requireNamespace("cdata", quietly = TRUE))){
    stop("packages 'ggplot2', 'reshape2', 'plotly', and 'cdata' required for scatter plotting!")
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
        values = "green")
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
    fig <- fig %>% add_markers()
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

#' Parallel Coordinate Plots
#'
#' The `pcp()` function for hyperdimensional data visualization,
#' which represents a p-dimensional data point in Cartesian coordinates by a
#' polyline (or curve) intercepting n-parallel axes, where p or the x-axis
#' represents the fitness values and n or the y-axis represents the objectives.
#'
#' @param object An object of nsga-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @examples
#' #Four Objectives Plotting
#' dtlz1 <- function (x, nobj = 4){
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
#' #Not Run
#' \dontrun{
#' result <- nsga3(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = c(0,0,0,0),
#'                 upper = c(1,1,1,1),
#'                 popSize = 92,
#'                 n_partitions = 12,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#' #Not Run
#' \dontrun{
#' pcp(object = result)
#' }
#'
#' @export
pcp <- function(object) {
  if(!all(requireNamespace("ggplot2", quietly = TRUE),
    requireNamespace("reshape2", quietly = TRUE),
    requireNamespace("dplyr", quietly = TRUE)))
    stop("packages 'ggplot2', 'dplyr' and 'reshape2' required for pcp plotting!")
  nObj <- ncol(object@fitness)
  colnames(object@fitness) <- sprintf("f_%s",seq(nObj))
  fitness <- reshape2::melt(object@fitness)
  fitness$color <- rainbow(object@popSize)
  fitness <- dplyr::rename(fitness,
    'Objective_Value' = value,
    'Objective_No' = Var2)
  ggplot2::ggplot(fitness, aes(x = Objective_No,
                               y = Objective_Value,
                               group = Var1, colour=factor(color))) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::theme_classic()
}

#' Heatmap Plots
#'
#' The `heat_map()` function for hyperdimensional data
#' visualization, which shows magnitude of a phenomenon as color in two
#' dimension.
#'
#' @param fitness An matrix of values representing the fitness of the objective
#' values of nsga-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @examples
#' #Four Objectives Plotting
#' dtlz1 <- function (x, nobj = 4){
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
#' #Not Run
#' \dontrun{
#' result <- nsga3(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = rep(0,4),
#'                 upper = rep(1,4),
#'                 popSize = 92,
#'                 n_partitions = 12,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#' #Not Run
#' \dontrun{
#' heat_map(fitness = result@fitness)
#' }
#'
#' @export
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

# heat_map <- function(fitness){
#   if (is.null(dim(fitness))){
#     fitness <- t(matrix(fitness))
#   }
#   nObj <- ncol(fitness)
#   colnames(fitness) <- sprintf("f_%s",seq(nObj))
#   fitness <- reshape2::melt(fitness)
#   fitness <- dplyr::rename(fitness,
#     'Pop' = Var1,
#     'Objective_Value' = value,
#     'Objective_No' = Var2)
#   ggplot2::ggplot(fitness, aes(x = Objective_No,
#     y = Pop,
#     fill = Objective_Value)) +
#     ggplot2::geom_raster() +
#     ggplot2::scale_y_continuous(labels = unique(as.character(fitness$Pop)),
#                                 breaks = unique(fitness$Pop))
# }

#' Polar Area Plot
#'
#' The `polar()` function is a viable tool for one dimesiona data
#' visualization, which that shows magnitude of a phenomenon as color in two
#' dimension.
#'
#' @param fitness An matrix of values representing the fitness of the objective
#' values of nsga-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @examples
#' #Four Objectives Plotting
#' dtlz1 <- function (x, nobj = 4){
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
#' #Not Run
#' \dontrun{
#' result <- nsga3(type = "real-valued",
#'                 fitness = dtlz1,
#'                 lower = rep(0,4),
#'                 upper = rep(1,4),
#'                 popSize = 92,
#'                 n_partitions = 12,
#'                 monitor = FALSE,
#'                 maxiter = 500)
#' }
#' #Not Run
#' \dontrun{
#' polar(fitness = result@fitness)
#' }
#'
#' @export
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

# polar <- function(fitness) {
#   if (is.null(dim(fitness))){
#     fitness <- t(matrix(fitness))
#   }
#   nObj <- ncol(fitness)
#   colnames(fitness) <- sprintf("f_%s",seq(nObj))
#   fitness <- reshape2::melt(fitness)
#   fitness <- dplyr::rename(fitness,
#     'Pop' = Var1,
#     'Objective_Value' = value,
#     'Objective_No' = Var2)
#   ggplot2::ggplot(fitness, aes(x = Objective_No,
#     y = Objective_Value,
#     fill = Objective_No)) +
#     ggplot2::geom_bar(width = 1, stat="identity") +
#     ggplot2::coord_polar() + ggplot2::theme_light() +
#     ggplot2::facet_wrap(~Pop, nrow = 1)
# }


utils::globalVariables(c("Pop","Objective_No", "Objective_Value", "Var1", "Var2", "color", "columns", "f_1", "f_2", "label_both", "rows", "value", "x", "y"))
