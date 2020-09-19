#' Niche-Preservation Operation
#'
#' Generation of niche, by associating reference points to population members
#'
#' Niching procesure is a algorithms proposed by K. Deb and H. Jain in 2013.
#'
#'
#' @param pop Last Front Population
#' @param n_remaining Number of points to choose
#' @param niche_count Niche count of individuals with the reference point
#' @param niche_of_individuals Count of the closest reference point to the last front objective values
#' @param dist_to_niche Distance between closest reference point to last front objective values
#'
#' @author Francisco Benitez
#' \email{benitez.fj@@hotmail.com}
#'
#' @references K. Deb and H. Jain, 'An Evolutionary Many-Objective Optimization Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I: Solving Problems With Box Constraints,' in IEEE Transactions on Evolutionary Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014, doi: 10.1109/TEVC.2013.2281535.
#'
#' Scrucca L. (2013). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37,
#' \url{http://www.jstatsoft.org/v53/i04/}.
#'
#' Felix-Antoine Fortin, Francois-Michel De Rainville, Marc-André Gardner Gardner, Marc Parizeau, and Christian Gagne. 2012. DEAP: evolutionary algorithms made easy. J. Mach. Learn. Res. 13, 1 (January 2012), 2171–2175.
#'
#'
#' @seealso [associate_to_niches()], [PerformScalarizing()]
#'
#' @return Returns the association of reference points to each individual in the population.
#' @export
niching <- function(pop, n_remaining, niche_count, niche_of_individuals, dist_to_niche) {
    survivors <- c()
    mask <- rep(TRUE, nrow(pop))
    while (length(survivors) < n_remaining) {
        n <- n_remaining - length(survivors)

        available_niches <- rep(FALSE, length(niche_count))
        available_niches[unique(niche_of_individuals[mask])] <- TRUE

        min_count <- min(niche_count[available_niches])

        selected_niches <- which((niche_count == min_count) & available_niches)

        if (length(selected_niches) > 1) {
            selected_niches <- sample(selected_niches)
        }
        selected_niches <- as.vector(na.omit(selected_niches[1:n]))

        for (i in selected_niches) {
            niche_of_individual <- which((niche_of_individuals == i) & mask)
            if (length(niche_of_individual) > 1) {
                niche_of_individual <- sample(niche_of_individual)
            }

            if (niche_count[i] == 0) {
                s <- niche_of_individual[which.min(dist_to_niche[niche_of_individual])]
            } else {
                s <- niche_of_individual[1]
            }

            mask[s] <- FALSE
            niche_count[i] <- niche_count[i] + 1
            survivors <- c(survivors, s)

        }
    }
    return(survivors)
}





# niching <- function(pop, n_remaining, niche_count, niche_of_individuals, dist_to_niche){
#   survivors <- c()
#   mask <- rep(TRUE, nrow(pop))
#   while (length(survivors) < n_remaining) {
#     n_select <- n_remaining - length(survivors)
#     next_niches_list <- unique(niche_of_individuals[mask])
#     next_niche_count <- niche_count[next_niches_list]
#     min_niche_count <- min(next_niche_count) #Traemos todos los nichos con el recuento minimo
#     next_niches <- next_niches_list[which(next_niche_count == min_niche_count)]
#     next_niches <- as.vector(na.omit(next_niches[sample(length(next_niches))[1:n_select]]))
#     # if(length(next_niches) > 1){
#     # next_niches <- next_niches[seq(length(next_niches))[n_select]]
#     # }
#     for (i in next_niches) {
#     #next_ind <- which(((niche_of_individuals == i) == mask))
#       next_ind <- which('&'((niche_of_individuals == i),mask))
#       if (length(next_ind)>1) {
#         next_ind <- sample(next_ind)
#       }
#       if (niche_count[i] == 0) {
#         next_ind <- next_ind[which.min(dist_to_niche[next_ind] == min(dist_to_niche[next_ind]))]
#       } else { #Ya randomizado
#         next_ind = next_ind[1]
#       }
#       mask[next_ind] <- FALSE
#       survivors <- c(survivors,next_ind)
#       niche_count[i] <- niche_count[i] + 1
#     }
#   }
#   return(survivors)
# }


# niching <- function(pop, n_remaining, niche_count, niche_of_individuals, dist_to_niche){
#   survivors <- c()
#   mask <- rep(TRUE, nrow(pop))
#   while (length(survivors)<n_remaining) {
#     n_select <- n_remaining - length(survivors)
#     next_niches_list <- unique(niche_of_individuals[mask])
#     next_niche_count <- niche_count[next_niches_list]
#     min_niche_count <- min(next_niche_count) #Traemos todos los nichos con el recuento minimo
#     next_niches <- next_niches_list[which(next_niche_count == min_niche_count)]
#     next_niches <- next_niches[sample(length(next_niches))[n_select]]
#     for (i in next_niches) {
#       next_ind <- which(((niche_of_individuals == next_niches[i]) == mask))
#       if (length(next_ind)>1) {
#         next_ind <- sample(next_ind)
#       }
#       if (niche_count[next_niche] == 0) {
#         next_ind <- next_ind[np.argmin(dist_to_niche[next_ind])]
#       } else {
#         next_ind = next_ind[0]
#       }
#       mask[next_ind] = F
#       survivors <- c(survivors,next_ind)
#       niche_count[i] = niche_count[i]+1
#     }
#   }
# return(survivors)
# }
