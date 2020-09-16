
# ideal_point
UpdateIdealPoint <- function(object, nObj) {
    cost <- object@fitness
    if (anyNA(object@ideal_point)) {
        ideal_point <- c()
        ideal_point <- apply(object@fitness, 2, min)
    } else {
        ideal_point <- object@ideal_point
        cost <- rbind(ideal_point, cost)
        ideal_point <- apply(cost, 2, min)
    }
    return(ideal_point)
}

# worst_point
UpdateWorstPoint <- function(object, nObj) {
    cost <- object@fitness
    if (anyNA(object@worst_point)) {
        worst_point <- c()
        worst_point <- apply(object@fitness, 2, max)
    } else {
        worst_point <- object@worst_point
        cost <- rbind(worst_point, cost)
        worst_point <- apply(cost, 2, max)
    }
    return(worst_point)
}

# extreme_points
PerformScalarizing <- function(population, fitness, smin, extreme_points, ideal_point) {
    nPop <- nrow(population)
    nObj <- ncol(fitness)
    if (!anyNA(smin)) {
        F <- rbind(extreme_points, fitness)
    } else {
        extreme_points <- matrix(0, nObj, nObj)
        smin <- rep(Inf, nObj)
        F <- fitness
    }
    fp <- sweep(F, 2, ideal_point)
    w <- diag(1, nObj)
    w[which(w == 0)] <- 1e-06
    for (j in 1:nObj) {
        s <- rep(0, nPop)
        for (i in 1:nPop) {
            s[i] <- max(fp[i, ]/w[j, ])
        }
        sminj <- min(s)
        ind <- which(s == sminj)

        if (length(ind) > 1)
            ind <- sample(ind, 1)

        if (sminj < smin[j]) {
            extreme_points[j, ] <- F[ind, ]
            smin[j] <- sminj
        }
    }
    out <- list(extremepoint = extreme_points, indexmin = smin)
    return(out)
}

# PerformScalarizing <- function(object) { nPop <- nrow(object@population) smin <- object@smin
# extreme_points <- object@extreme_points nObj <- ncol(object@fitness) ideal_point <- object@ideal_point if
# (!anyNA(smin)) { extreme_points <- object@extreme_points smin <- object@smin F <-
# rbind(object@extreme_points, object@fitness) } else { extreme_points <- matrix(0, nObj, nObj) smin <-
# rep(Inf,nObj) F <- object@fitness } fp = sweep(F,2,ideal_point) w = diag(1, nObj) w[which(w==0)] = 1e-6
# for (j in 1:nObj) { s <- rep(0, nPop) for (i in 1:nPop) { s[i] <- max(fp[i,]/w[j,]) } sminj <- min(s) ind
# <- which(s == sminj) if (length(ind)>1) ind <- sample(ind, 1) if (sminj < smin[j]) { extreme_points[j, ]
# <- F[ind, ] smin[j] <- sminj } } out <- list(extremepoint = extreme_points, indexmin = smin) return(out)
# }

# get_extreme_points <- function(object) { nObj <- ncol(object@fitness) nPop <- nrow(object@population) w
# <- diag(1, nObj) w[which(w==0)] = 1e-6 extreme_points <- object@extreme_points if
# (!is.na(extreme_points)) { smin <- object@smin F <- rbind(object@extreme_points, object@fitness) } else {
# extreme_points <- matrix(0, nObj, nObj) smin <- rep(Inf,nObj) F <- object@fitness } fp =
# sweep(F,2,ideal_point) fp[which(fp<(1e-3))] <- 0 for (j in 1:nObj) { s <- rep(0, nPop) for (i in 1:nPop)
# { s[i] <- max(fp[i,]*w[j,]) } sminj <- min(s) ind <- which(s == sminj) if (length(ind)>1) ind <-
# sample(ind, 1) if (sminj <= smin[j]) { extreme_points[j, ] <- F[ind, ] smin[j] <- sminj } } out <-
# list(extremepoint = extreme_points, indexmin = smin) return(out) }


# nadir_point
get_nadir_point <- function(object) {
    extreme_point <- object@extreme_points
    ideal_point <- object@ideal_point
    worst_point <- object@worst_point
    nObj <- ncol(object@fitness)
    worst_of_front <- object@worst_of_front
    worst_of_population <- object@worst_of_population
    out <- tryCatch({
        M <- sweep(extreme_point, 2, ideal_point)
        b <- rep(1, nObj)
        plane <- solve(M, b)
        intercepts <- 1/plane
        nadir_point <- ideal_point + intercepts
        if (!all.equal(as.vector(M %*% plane), b) || any(intercepts <= 1e-05) || any(nadir_point > worst_point)) {
            stop()
        }
        nadir_point
    }, error = function(e) {
        nadir_point <- worst_of_front
        return(nadir_point)
    })
    b <- (out - ideal_point) <= 1e-06
    out[b] <- worst_of_population[b]
    return(out)
}


