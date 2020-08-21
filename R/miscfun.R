# nsgaMonitor <- function(object, digits = getOption("digits"), ...)
# {
#   fitness   <- na.exclude(object@fitness)
#   sumryStat <- c(mean(fitness), max(fitness))
#   sumryStat <- format(sumryStat, digits = digits)
#   cat(paste("GA | iter =", object@iter,
#     "| Mean =", sumryStat[1],
#     "| Best =", sumryStat[2]))
#   cat("\n")
#   flush.console()
# }

# garun <- function(x)
# {
#   x <- as.vector(x)
#   sum(rev(x) >= (max(x, na.rm = TRUE) - gaControl("eps")))
# }


# nsgaMonitor <- function(object, number_objective, ...) {
#   fitness <- object@fitness
#   if (nObj==3) {
#     Y <- object@f[[1]]
#     first_front <- fitness[Y,]
#     rgl::plot3d(fitness)
#     rgl::plot3d(first_front, col="red", size=8, add=TRUE)
#     rgl::plot3d(x = min(first_front[,1]), y = min(first_front[,2]), z = min(first_front[,3]),
#       col="green", size=8, add=TRUE)
#     rgl::bgplot3d({plot.new(); title(main = iter, line = 3);});
#     fitness.range <- diff(apply(fitness,2,range))
#     #Sys.sleep(0.2)
#   } else if (nObj==2) {
#     first_front <- fitness[object@f[[1]],]
#     par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
#     plot(fitness[,1], fitness[,2],
#          col = "green", pch = 20,
#          main= paste("Iter: ", object@iter),
#          xlim = range(fitness[, 1]), ylim = range(fitness[, 2]))
#     lines(first_front[,1][order(first_front[,1])], first_front[,2][order(first_front[,1])],
#       xlim=range(first_front[,1]), ylim=range(first_front[,2]),
#       xlab="f1", ylab="f2", col = "red", type="l" ,pch = 12 ,main= "Pareto Front")
#     # legend(5,10,c("Population","Pareto Optimal"),
#     #        lwd=c(5,2), col=c("green","red"), y.intersp=1.5)
#     legend("topright", inset=c(-.80,0),
#       legend=c("Fitness Values","Pareto Optimal"), pch=c(19,NA),
#       title="Values", lwd=c(NA,2), col=c("green","red"), y.intersp=1.5)
#     Sys.sleep(0.25)
#   }
#   # cat(paste("NSGA | iteration =", object@iter,
#   #           "| First Front =", firt_front))
#   list("NSGA | iteration" = iter,
#     "| First Front" = first_front)
# }

nsgaMonitor <- function(object, number_objectives, ...) {
  fitness <- object@fitness
  iter <- object@iter
  if (number_objectives == 3) {
    # Y <- object@f[[1]]
    # first_front <- fitness[Y,]
    rgl::plot3d(fitness)
    # rgl::plot3d(first_front, col="red", size=8, add=TRUE)
    # rgl::plot3d(x = min(first_front[,1]),
    #             y = min(first_front[,2]),
    #             z = min(first_front[,3]),
    #   col="green", size=8, add=TRUE)
    rgl::bgplot3d({plot.new(); title(main = paste("Iter: ", iter), line = 3);});
    # fitness.range <- diff(apply(fitness,2,range))
    #scatterplot3d::scatterplot3d(fitness, main = paste("Iter: ", iter), highlight.3d = TRUE, pch = 20)
    #Sys.sleep(0.2)
  } else if (number_objectives == 2) {
    first_front <- fitness[object@f[[1]],]
    par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(fitness[,1], fitness[,2],
         col = "green", pch = 20,
         main= paste("Iter: ", object@iter),
         xlim = range(fitness[, 1]), ylim = range(fitness[, 2]))
    lines(first_front[,1][order(first_front[,1])], first_front[,2][order(first_front[,1])],
          xlim=range(first_front[,1]), ylim=range(first_front[,2]),
          xlab="f1", ylab="f2", col = "red", type="l" ,pch = 12 ,main= "Pareto Front")
    # legend(5,10,c("Population","Pareto Optimal"),
    #        lwd=c(5,2), col=c("green","red"), y.intersp=1.5)
    legend("topright", inset=c(-.80,0),
           legend=c("Population","Pareto Optimal"), pch=c(19,NA),
           title="Values", lwd=c(NA,2), col=c("green","red"), y.intersp=1.5)
    #Sys.sleep(0.25)
  }
  # cat(paste("NSGA | iteration =", object@iter,
  #           "| First Front =", firt_front))
  list("NSGA | iteration" = iter,
       "| First Front" = first_front)
}

nsgaSummary <- function(object, ...) {
  if (class(object)[1] == "nsga") {

  }
  if (class(object)[1] == "nsga2") {
    first <- object@f[[1]]
    first_front_fit <- object@fitness[first, ]
    first_front_pop <- object@population[first, ]
    first_cd <- object@crowdingDistance[first, ]
    # compute summary for each step
    data.frame(first_front_fit = first_front_fit,
               first_front_pop = first_front_pop,
               crowding_dist = first_cd)
  }
  if (class(object)[1] == "nsga3") {

  }
}
