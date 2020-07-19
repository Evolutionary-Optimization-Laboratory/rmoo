
associate_to_niches <- function(object, utopian_epsilon = 0.0) {
  fitness <- object@fitness
  ideal_point <- object@ideal_point
  niches <- object@reference_points
  nadir_point <- object@nadir_point

  utopian_point = ideal_point - utopian_epsilon
  denom = nadir_point - utopian_point
  denom[which(denom==0)] = 1*10^(-12)

  delta = sweep(fitness, 2,  utopian_point)
  N = sweep(delta, 2, denom,FUN = "/")
  dist_matrix = compute_perpendicular_distance(N, niches)

  niche_of_individuals <- dist_to_niche <-  c()
  for (i in 1:nrow(dist_matrix)) {
    niche_of_individuals[i] <- which(dist_matrix[i,] == min(dist_matrix[i,]))
  }

  for (i in 1:nrow(fitness)) {
    dist_to_niche[i] <- dist_matrix[i, niche_of_individuals[i]]
  }

  out <- list(distance = dist_to_niche,
              niches = niche_of_individuals)
  return(out)
}


compute_perpendicular_distance <- function(N, ref_dirs) {
  u <- do.call(rbind, replicate(nrow(N),ref_dirs, simplify = FALSE))
  v <- matrix(rep(N, each=nrow(ref_dirs)), ncol = ncol(N))
  norm_u <- asd <- val <- c()

  for (i in 1:nrow(u)) {
    norm_u[i] <- norm(u[i,], type="2")
  }
  aux <- v * u
  for (i in 1:nrow(aux)) {
    asd[i] <- sum(aux[i, ])
  }
  aux <- c()
  scalar_proj = asd / norm_u
  proj = (scalar_proj * u) / norm_u
  aux <- proj - v
  for (i in 1:nrow(aux)) {
    val[i] <- norm(aux[i, ], type="2")
  }
  m <- matrix(val, nrow = nrow(N), ncol = nrow(ref_dirs), byrow = TRUE)
  return(m)
}
