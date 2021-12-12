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
#' @param niche_of_individuals Count of the closest reference point to the last
#' front objective values
#' @param dist_to_niche Distance between closest reference point to last front
#' objective values
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references K. Deb and H. Jain, 'An Evolutionary Many-Objective Optimization
#' Algorithm Using Reference-Point-Based Nondominated Sorting Approach, Part I:
#' Solving Problems With Box Constraints,' in IEEE Transactions on Evolutionary
#' Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014,
#' doi: 10.1109/TEVC.2013.2281535.
#'
#' Scrucca, L. (2017) On some extensions to 'GA' package: hybrid optimisation,
#' parallelisation and islands evolution. The R Journal, 9/1, 187-206.
#' doi: 10.32614/RJ-2017-008
#'
#' Felix-Antoine Fortin, Francois-Michel De Rainville, Marc-André Gardner
#' Gardner, Marc Parizeau, and Christian Gagne. 2012. DEAP: evolutionary
#' algorithms made easy. J. Mach. Learn. Res. 13, 1 (January 2012), 2171–2175.
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
