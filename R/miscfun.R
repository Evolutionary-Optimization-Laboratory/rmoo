
nsgaMonitor <- function(object, number_objectives, ...) {
    fitness <- object@fitness
    iter <- object@iter
    cl <- rainbow(object@popSize)
    
    if (number_objectives == 3) {
        rgl::plot3d(fitness)
        rgl::bgplot3d({
            plot.new()
            title(main = paste(class(object)[1], "Iter: ", iter), line = 3)
        })
        plot(seq(number_objectives), object@fitness[1, ], col = cl[1], type = "l", main = paste(class(object)[1], 
            "Iter: ", iter), ylim = c(min(object@fitness), max(object@fitness)), xlab = "Objective No", ylab = "Objective Value")
        for (i in 2:(object@popSize)) {
            lines(seq(number_objectives), object@fitness[i, ], col = cl[i], type = "l")
        }
        
    } else if (number_objectives == 2) {
        first_front <- fitness[object@f[[1]], ]
        par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)
        plot(fitness[, 1], fitness[, 2], col = "green", pch = 20, main = paste(class(object)[1], "Iter: ", 
            iter), xlim = range(fitness[, 1]), ylim = range(fitness[, 2]))
        lines(first_front[, 1][order(first_front[, 1])], first_front[, 2][order(first_front[, 1])], xlim = range(first_front[, 
            1]), ylim = range(first_front[, 2]), xlab = "f1", ylab = "f2", col = "red", type = "l", pch = 12, 
            main = "Pareto Front")
        legend("topright", inset = c(-0.8, 0), legend = c("Population", "Pareto Optimal"), pch = c(19, NA), 
            title = "Values", lwd = c(NA, 2), col = c("green", "red"), y.intersp = 1.5)
        
    } else if (number_objectives > 3) {
        plot(seq(number_objectives), object@fitness[1, ], col = cl[1], type = "l", main = paste(class(object)[1], 
            "Iter: ", iter), ylim = c(min(object@fitness), max(object@fitness)), xlab = "Objective No", ylab = "Objective Value")
        for (i in 2:(object@popSize)) {
            lines(seq(number_objectives), object@fitness[i, ], col = cl[i], type = "l")
        }
        
    }
}

nsgaSummary <- function(object, ...) {
    if (class(object)[1] == "nsga") {
        
    }
    if (class(object)[1] == "nsga2") {
        first <- object@f[[1]]
        first_front_fit <- object@fitness[first, ]
        first_front_pop <- object@population[first, ]
        first_cd <- object@crowdingDistance[first, ]
        hv <- ecr::computeHV(t(object@fitness[first, ]))
        # result <- hv
        result <- data.frame(`First Front Fit` = first_front_fit, `First Front Pop` = first_front_pop, `Crowding Dist` = first_cd, 
            Hypervolumen = hv)
        # cat('Iter: ', object@iter, 'Hypervolumen: ', hv, '\n' )
    }
    if (class(object)[1] == "nsga3") {
        first <- object@f[[1]]
        first_front_fit <- object@fitness[first, ]
        first_front_pop <- object@population[first, ]
        ideal_point <- object@ideal_point
        worst_point <- object@worst_point
        extreme_points <- object@extreme_points
        gd <- ecr::computeGenerationalDistance(t(object@fitness), t(object@reference_points))
        igd <- ecr::computeInvertedGenerationalDistance(t(object@fitness), t(object@reference_points))
        hv <- ecr::computeHV(t(object@fitness))
        metric <- data.frame(Iternation = object@iter, Generational_Distance = gd, Inverse_Generational_Distance = igd, 
            Hypervolumen = hv)
        result <- list(first_front_fit = first_front_fit, first_front_pop = first_front_pop, ideal_point = ideal_point, 
            worst_point = worst_point, extreme_points = extreme_points, metrics = metric)
    }
    return(result)
}
