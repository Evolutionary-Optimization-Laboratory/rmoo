#' @export
associate_to_niches <- function(object, utopian_epsilon = 0) {
    fitness <- object@fitness
    ideal_point <- object@ideal_point
    niches <- object@reference_points
    nadir_point <- object@nadir_point
    utopian_point <- ideal_point - utopian_epsilon
    denom <- nadir_point - utopian_point
    denom[which(denom == 0)] <- 1 * 10^(-12)

    delta <- sweep(fitness, 2, utopian_point)
    N <- sweep(delta, 2, denom, FUN = "/")
    dist_matrix <- 1 - compute_perpendicular_distance(N, niches)
    dist_matrix <- do.call(cbind, replicate(nrow(niches), sqrt(rowSums(fitness^2)), simplify = FALSE)) * sqrt(1 - dist_matrix^2)
    niche_of_individuals <- dist_to_niche <- c()
    niche_of_individuals <- apply(dist_matrix, 1, which.min)

    for (i in 1:nrow(fitness)) {
        dist_to_niche[i] <- dist_matrix[i, niche_of_individuals[i]]
    }

    out <- list(distance = dist_to_niche, niches = niche_of_individuals)
    return(out)
}

#' @export
compute_perpendicular_distance <- function(x, y) {
    p <- ncol(x)
    xx <- sqrt(rowSums(x^2))
    x <- x / matrix(rep(xx, p), ncol = p)
    yy <- sqrt(rowSums(y^2))
    y <- y / matrix(rep(yy, p), ncol = p)
    D = x %*% t(y)
    return(1 - D)
}

#' @export
compute_niche_count <- function(n_niches, niche_of_individuals) {
    niche_count <- rep(0, n_niches)
    test <- split(seq_along(niche_of_individuals), niche_of_individuals)
    a <- rbind(as.numeric(names(test)), unlist(lapply(test, length), use.names = FALSE))
    niche_count[a[1, ]] <- a[2, ]
    return(niche_count)
}


# compute_perpendicular_distance <- function(N, ref_dirs) {
#   u <- do.call(rbind, replicate(nrow(N), ref_dirs, simplify = FALSE))
#   v <- matrix(rep(N, each=nrow(ref_dirs)), ncol = ncol(N))
#   norm_u <- asd <- val <- c()
#   for (i in 1:nrow(u)) {
#     norm_u[i] <- norm(u[i,], type='2')
#   }
#   aux <- v * u
#   for (i in 1:nrow(aux)) {
#     asd[i] <- sum(aux[i, ])
#   }
#   aux <- c() scalar_proj <- asd / norm_u
#   proj <- (scalar_proj * u) / norm_u
#   aux <- proj - v
#   for (i in 1:nrow(aux)) {
#     val[i] <- norm(aux[i, ], type='2')
#   }
#   m <- matrix(val, nrow = nrow(N), ncol = nrow(ref_dirs), byrow = TRUE)
#   return(m)
# }
