
#ideal_point
UpdateIdealPoint <- function(object, nObj) {
  cost <- object@fitness
  if (is.null(object@ideal_point)) {
    ideal_point <- c()
    #nObj <- ncol(cost)
    for (i in 1:nObj) {
      ideal_point[i] <- min(cost[,i])
    }
  }else{
    ideal_point <- object@ideal_point
    cost <- rbind(ideal_point, cost)
    for (i in 1:nObj) {
      ideal_point[i] <- min(cost[,i])
    }
  }
  return(ideal_point)
}

#worst_point
UpdateWorstPoint <- function(object, nObj){
  cost <- object@fitness
  if(is.null(object@worst_point)){
    worst_point <- c()
    for (i in 1:nObj) {
      worst_point[i] <- max(cost[,i])
    }
  }else{
    worst_point <- object@worst_point
    cost <- rbind(worst_point, cost)
    for (i in 1:nObj) {
      worst_point[i] <- max(cost[,i])
    }
  }
  return(worst_point);
}

#extreme_points
PerformScalarizing <- function(object){
  nPop <- object@popSize;
  smin <- object@smin;
  extreme_points <- object@extreme_points;
  nObj <- ncol(object@popSize)
  ideal_point <- object@idealpoint;
  if(!is.null(object@smin)){
    extreme_points <- object@extreme_points;
    smin <- object@smin;
    F <- rbind(object@extreme_points, object@fitness);
  }else{
    extreme_points <-  matrix(0, nObj, nObj)
    smin <-  rep(Inf,nObj)
    F <- object@fitness
  }
  fp = sweep(F,2,ideal_point)
  w = diag(1, nObj)
  w[which(w==0)] = 1*10^(-6)
  for (j in 1:nObj) {
    s <- rep(0, nPop);
    for (i in 1:nPop) {
      s[i] <- max(fp[i,]/w[j,]);
    }
    sminj <- min(s);
    ind <- which(s == sminj);
    if (sminj < smin[j]){
      extreme_points[j,] <- F[ind,];
      smin[j] <- sminj;
    }
  }
  out <- list(extremepoint = extreme_points,
    indexmin = smin)
  return(out)
}

#nadir_point
get_nadir_point <- function(object) {
  extreme_point <- object@extremepoint
  ideal_point <- object@idealpoint
  worst_point <- object@worstpoint
  nObj <- object@nObj
  worst_of_front <- object@worst_of_front
  worst_of_population <- object@worst_of_population
  out <- tryCatch({
    M <- sweep(extreme_point,2,ideal_point)
    b <- rep(1, nObj)
    plane <- solve(M,b)
    intercepts <- 1 / plane
    nadir_point <- ideal_point + intercepts
    if (!all.equal(as.vector(M%*%plane), b) || any(intercepts <= 10e-6) || any(nadir_point > worst_point)){
      stop()
    }
    nadir_point
  },error = function(e)
  {
    nadir_point <- worst_of_front
    return(nadir_point)
  }
  )
  b <- (out - ideal_point) <= 1e-6
  out[b] <- worst_of_population[b]
  return(out)
}
