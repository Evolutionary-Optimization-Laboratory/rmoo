# Association Operator

# Association process of each member of the population with the reference points.

# The association operator will work with the normalized space, reference points
# and population members. First, generating the reference lines corresponding to
# each reference point and then calculating the perpendicular distance of each
# population member from each reference line.
# This code section corresponds to Algorithm 3 of the referenced paper.

#' @export
associate_to_niches <- function(object, utopian_epsilon = 0) {
  fitness <- object@fitness
  ideal_point <- object@ideal_point
  niches <- object@reference_points
  nadir_point <- object@nadir_point
  utopian_point <- ideal_point - utopian_epsilon
  denom <- nadir_point - utopian_point
  denom[denom == 0] <- 1 * 10^(-12)

  delta <- sweep(fitness, 2, utopian_point)
  N <- sweep(delta, 2, denom, FUN = "/")
  dist_matrix <- 1 - compute_perpendicular_distance(N, niches)

  # Use the C++ function to compute dist_matrix
  # dist_matrix <- 1 - compute_perpendicular_distance_cpp(N, niches)

  dist_matrix <- do.call(cbind,
                         replicate(nrow(niches),
                                   sqrt(rowSums(fitness^2)),
                                   simplify = FALSE)) * sqrt(1 - dist_matrix^2)

  dist_to_niche <- apply(dist_matrix, 1, min)
  niche_of_individuals <- apply(dist_matrix, 1, which.min)

  dist_to_niche <- dist_matrix[cbind(seq_along(niche_of_individuals), niche_of_individuals)]

  out <- list(distance = dist_to_niche, niches = niche_of_individuals)
  return(out)
}
# associate_to_niches <- function(object, utopian_epsilon = 0) {
#     fitness <- object@fitness
#     ideal_point <- object@ideal_point
#     niches <- object@reference_points
#     nadir_point <- object@nadir_point
#     utopian_point <- ideal_point - utopian_epsilon
#     denom <- nadir_point - utopian_point
#     denom[which(denom == 0)] <- 1 * 10^(-12)
#
#     delta <- sweep(fitness, 2, utopian_point)
#     N <- sweep(delta, 2, denom, FUN = "/")
#     dist_matrix <- 1 - compute_perpendicular_distance(N, niches)
#     dist_matrix <- do.call(cbind,
#                            replicate(nrow(niches),
#                                      sqrt(rowSums(fitness^2)),
#                                      simplify = FALSE)) * sqrt(1 - dist_matrix^2)
#     niche_of_individuals <- dist_to_niche <- c()
#     niche_of_individuals <- apply(dist_matrix, 1, which.min)
#
#     # write.csv(fitness,file="fitness.csv")
#     # write.csv(N,file="N.csv")
#     # write.csv(niches,file="niches.csv")
#
#     for (i in 1:nrow(fitness)) {
#       # write.csv(dist_matrix,file="dist_matrix.csv")
#       # write.csv(i,file="i.csv")
#       # write.csv(niche_of_individuals[i],file="niche_of_individualsi.csv")
#       # write.csv(niche_of_individuals,file="niche_of_individuals.csv")
#       dist_to_niche[i] <- dist_matrix[i, niche_of_individuals[i]]
#     }
#
#     out <- list(distance = dist_to_niche, niches = niche_of_individuals)
#     return(out)
# }


#' @export
compute_perpendicular_distance <- function(x, y) {
  xx <- sqrt(rowSums(x^2))
  x <- x / xx
  yy <- sqrt(rowSums(y^2))
  y <- y / yy
  D <- x %*% t(y)
  return(1 - D)
}
# compute_perpendicular_distance <- function(x, y) {
#     p <- ncol(x)
#     xx <- sqrt(rowSums(x^2))
#     x <- x / matrix(rep(xx, p), ncol = p)
#     yy <- sqrt(rowSums(y^2))
#     y <- y / matrix(rep(yy, p), ncol = p)
#     D = 1 - x %*% t(y)
#     return( D)
# }

# cppFunction('NumericMatrix compute_perpendicular_distance_cpp(NumericMatrix x, NumericMatrix y) {
#   int p = x.ncol();
#   int n = x.nrow();
#   int m = y.nrow();
#   NumericMatrix D(n, m);
#
#   for (int i = 0; i < n; i++) {
#     for (int j = 0; j < m; j++) {
#       double sum = 0.0;
#       for (int k = 0; k < p; k++) {
#         sum += x(i, k) * y(j, k);
#       }
#       D(i, j) = 1 - sum;
#     }
#   }
#
#   return D;
# }')

#' @export
compute_niche_count <- function(n_niches, niche_of_individuals) {
  return(tabulate(niche_of_individuals, nbins = n_niches))
}
# compute_niche_count <- function(n_niches, niche_of_individuals) {
#     niche_count <- rep(0, n_niches)
#     test <- split(seq_along(niche_of_individuals), niche_of_individuals)
#     a <- rbind(as.numeric(names(test)), unlist(lapply(test, length), use.names = FALSE))
#     niche_count[a[1, ]] <- a[2, ]
#     return(niche_count)
# }

