#' Calculate of Non-Dominated Front
#'
#' A fast approach for calculate Non-Dominated Fronts.
#'
#' Function to determine the non-dominated fronts of a population and the
#' aptitude value.
#'
#' @param object An object of class 'nsga', usually resulting from a call to
#' function nsga, nsga2 and nsga3.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references K. Deb, A. Pratap, S. Agarwal and T. Meyarivan, 'A fast and
#' elitist multiobjective genetic algorithm: NSGA-II,' in IEEE Transactions on
#' Evolutionary Computation, vol. 6, no. 2, pp. 182-197, April 2002,
#' doi: 10.1109/4235.996017.
#'
#' @seealso [nsga()], [nsga2()] and [nsga3()]
#'
#' @return A list with 'non-dominated fronts' and 'occupied positions' on the fronts.
#' @export
non_dominated_fronts <- function(object) {
    pop_count <- 0
    pop_size <- nrow(object@population)
    dominated_count <- vector("list", pop_size)
    domination_set <- vector("list", pop_size)
    front <- vector("list", pop_size)
    fitness <- object@fitness
    q <- c()
    front_index <- 1

    for (i in seq_len(pop_size)) {
        dominated_count[[i]] <- 0
    }
    for (i in seq_len(pop_size)) {
        for (j in seq_len(pop_size)) {
            if (i != j & i < j) {
                if (all(fitness[i, ] <= fitness[j, ]) && any(fitness[i, ] < fitness[j, ])) {
                  domination_set[[i]] <- c(domination_set[[i]], j)
                  dominated_count[[j]] <- dominated_count[[j]] + 1

                }
                if (all(fitness[j, ] <= fitness[i, ]) && any(fitness[j, ] < fitness[i, ])) {
                  domination_set[[j]] <- c(domination_set[[j]], i)
                  dominated_count[[i]] <- dominated_count[[i]] + 1
                }
            }
        }

        if (dominated_count[[i]] == 0) {
            q <- c(q, i)
            front[[i]] <- front_index
            pop_count <- (pop_count + 1)
        }
    }
    f <- list()
    f[front_index] <- list(sort(q))

    while (TRUE) {
        q <- c()
        for (i in f[[front_index]]) {
            for (j in domination_set[[i]]) {
                dominated_count[[j]] <- dominated_count[[j]] - 1
                if (dominated_count[j] == 0) {
                  q <- c(q, j)
                  front[[j]] <- front_index + 1
                  pop_count <- pop_count + 1
                }
            }
        }
        if (is.null(q))
            break
        front_index <- front_index + 1
        f[front_index] <- list(sort(q))
    }
    out <- list(fit = f, fronts = front)
    return(out)
}




# nondominatedfronts <- function(population, popSize, fitness) {
#   pop_count <- 0
#   DominatedCount <- vector('list', popSize)
#   DominationSet <- vector('list', popSize)
#   front <- vector('list', popSize)
#   q <- c()
#   front_index <- 1
#   for (i in seq_len(popSize)) {
#     DominatedCount[[i]] <- 0
#   }
#   for (i in seq_len(popSize)) {
#     for (j in seq_len(popSize)) {
#       if(i != j & i < j) {
#         if (all(fitness[i, ] <= fitness[j, ]) &&
#              any(fitness[i, ] < fitness[j, ])) {
#           DominationSet[[i]] <- c(DominationSet[[i]], j)
#           DominatedCount[[j]] <- DominatedCount[[j]] + 1
#         }
#         if (all(fitness[j, ] <= fitness[i, ]) &&
#             any(fitness[j, ] < fitness[i, ])) {
#           DominationSet[[j]] <- c(DominationSet[[j]], i)
#           DominatedCount[[i]] <- DominatedCount[[i]] + 1
#         }
#       }
#     }
#     if (DominatedCount[[i]] == 0){
#       q <- c(q, i)
#       front[[i]] <- front_index
#       pop_count <- (pop_count + 1)
#     }
#   }
#   f <- list()
#   f[front_index] <- list(sort(q))
#   while (TRUE) {
#     q <- c()
#     for (i in f[[front_index]]) {
#       for(j in DominationSet[[i]]) {
#         DominatedCount[[j]] <- DominatedCount[[j]] - 1
#         if (DominatedCount[j] == 0) {
#           q <- c(q, j)
#           front[[j]] <- front_index + 1
#           pop_count <- (pop_count + 1)
#         }
#       }
#     }
#     if (is.null(q)) break
#     front_index <- front_index + 1
#     f[front_index] <- list(sort(q))
#   }
#   out <- list(F = f,
#               Front = front)
#   return(out)
# }
