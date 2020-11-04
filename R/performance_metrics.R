#' @export
generational_distance <- function(fitness, reference_points) {
    popSize <- nrow(fitness)
    distances <- rep(Inf, popSize)
    aux <- c()
    for (i in 1:popSize) {
        for (j in 1:popSize) {
            aux <- dist(rbind(fitness[i, ], reference_points[j, ]),
                        method = "euclidean")
            if (aux < distances[i]) {
                distances[i] <- aux
            }
        }
        aux <- c()
    }
    distances <- sum(distances) / nrow(reference_points)

    return(distances)
}
