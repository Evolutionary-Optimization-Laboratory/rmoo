#' Scatter Plot Functions
#'
#' Allows to make scatter plots in publication quality allowing
#' to represent 2-D, 3-D and M-D
#'
#' @param object An object of nsga3-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
#' @param ... Other arguments passed on to methods. Used to pass the `optimal`
#' value of the objective function, in case of having it.
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
  algorithm <- class(object)[1]
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
    objective_values <- object@fitness
    colnames(objective_values) <- c("f_1", "f_2")
    colnames(optimal) <- c("f_1", "f_2")
    optimal <- as.data.frame(optimal)
    objective_values <- as.data.frame(objective_values)

    ggplot2::ggplot() + geom_line(data = optimal,
      aes(x = f_1,
        y = f_2,
        color = "Pareto Optimal")) +
      ggplot2::geom_point(data = objective_values,
        aes(x = f_1,
          y = f_2,
          color = "Objective_Value")) +
      ggplot2::labs(title = algorithm, color = "Values") +
      ggplot2::scale_color_manual(labels = c("Objective_Value", "Pareto Optimal"),
        values = c("green", "black"))

  } else {
    objective_values <- object@fitness
    colnames(objective_values) <- c("f_1", "f_2")
    objective_values <- as.data.frame(objective_values)

    ggplot2::ggplot() + ggplot2::geom_point(data = objective_values,
      aes(x = f_1,
        y = f_2,
        color = "Objective_Values")) +
      ggplot2::labs(title = algorithm,
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
      colors = c('#BF382A', '#0C4B8E'),
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
  colnames(fit) <- sprintf("Objective_%s",seq(nObj))
  meas_vars <- colnames(fit)

  controlTable <- data.frame(expand.grid(meas_vars, meas_vars,
                                         stringsAsFactors = FALSE))

  colnames(controlTable) <- c("x", "y")

  controlTable <- cbind(data.frame(pair_key = paste(controlTable[[1]],
                                                    controlTable[[2]]),
                                   stringsAsFactors = FALSE),
                        controlTable)

  fit_aug = cdata::rowrecs_to_blocks(fit, controlTable)

  splt <- strsplit(fit_aug$pair_key, split = " ", fixed = TRUE)
  fit_aug$columns <- vapply(splt, function(si) si[[1]], character(1))
  fit_aug$rows <- vapply(splt, function(si) si[[2]], character(1))

  fit_aug$columns <- factor(as.character(fit_aug$columns), meas_vars)
  fit_aug$rows <- factor(as.character(fit_aug$rows), meas_vars)

  ggplot2::ggplot(fit_aug, aes(x=x, y=y)) +
    ggplot2::geom_point(aes(color=rows, shape=columns)) +
    ggplot2::facet_grid(rows~columns, labeller = label_both, scale = "free") +
    ggplot2::ggtitle("Anderson's Iris Data -- 3 species") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL)
}



#' Parallel Coordinate Plots
#'
#' The `pcp()` function is a viable tool for hyperdimensional data visualization,
#' which represents a p-dimensional data point in Cartesian coordinates by a
#' polyline (or curve) intercepting n-parallel axes, where p or the x-axis
#' represents the fitness values and n or the y-axis represents the objectives.
#'
#' @param object An object of nsga3-class, nsga2-class or nsga3-class.
#' See [nsga-class], [nsga2-class] or [nsga3-class] for a description of
#' available slots information.
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
  temp <- reshape2::melt(object@fitness)
  temp$color <- rainbow(object@popSize)
  temp <- dplyr::rename(temp,
    'Objective_Value' = value,
    'Objective_No' = Var2)
  ggplot2::ggplot(temp,aes(x=Objective_No,
    y = Objective_Value,
    group=Var1, colour=factor(color))) +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::theme_classic()
}


utils::globalVariables(c("Objective_No", "Objective_Value", "Var1", "Var2", "color", "columns", "f_1", "f_2", "label_both", "rows", "value", "x", "y"))
