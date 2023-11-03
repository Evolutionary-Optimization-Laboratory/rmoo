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
  nondom.layers <- ecr::doNondominatedSorting(t(object@fitness))

  max.rank <- max(nondom.layers$ranks)

  idxs.by.rank <- lapply(seq(max.rank), function(r) which(nondom.layers$ranks == r))

  out <- list(fit = idxs.by.rank, fronts = nondom.layers$ranks)

  return(out)
}

# non_dominated_fronts <- function(object) {
#   fitness <- object@fitness
#   popSize <- nrow(fitness)
#   dominated_count <- numeric(popSize)
#   domination_set <- vector("list", popSize)
#   front <- numeric(popSize)
#   front_index <- 1
#
#   for (i in seq_len(popSize - 1)) {
#     for (j in seq(i + 1, popSize)) {
#       if (all(fitness[i, ] <= fitness[j, ]) && any(fitness[i, ] < fitness[j, ])) {
#         domination_set[[i]] <- c(domination_set[[i]], j)
#         dominated_count[j] <- dominated_count[j] + 1
#       } else if (all(fitness[j, ] <= fitness[i, ]) && any(fitness[j, ] < fitness[i, ])) {
#         domination_set[[j]] <- c(domination_set[[j]], i)
#         dominated_count[i] <- dominated_count[i] + 1
#       }
#     }
#   }
#
#   front_index <- 1
#   q <- which(dominated_count == 0)
#   front[q] <- front_index
#
#   while (length(q) > 0) {
#     front_index <- front_index + 1
#     new_q <- integer()
#     for (i in q) {
#       for (j in domination_set[[i]]) {
#         dominated_count[j] <- dominated_count[j] - 1
#         if (dominated_count[j] == 0) {
#           new_q <- c(new_q, j)
#           front[j] <- front_index
#         }
#       }
#     }
#     q <- new_q
#   }
#
#   out <- list(fit = split(seq_len(popSize), front), fronts = front)
#   return(out)
# }





# non_dominated_fronts <- function(object) {
#     pop_count <- 0
#     pop_size <- nrow(object@population)
#     dominated_count <- vector("list", pop_size)
#     domination_set <- vector("list", pop_size)
#     front <- vector("list", pop_size)
#     fitness <- object@fitness
#     q <- c()
#     front_index <- 1
#
#     for (i in seq_len(pop_size)) {
#         dominated_count[[i]] <- 0
#     }
#     for (i in seq_len(pop_size)) {
#         for (j in seq_len(pop_size)) {
#             if (i != j & i < j) {
#                 if (all(fitness[i, ] <= fitness[j, ]) && any(fitness[i, ] < fitness[j, ])) {
#                   domination_set[[i]] <- c(domination_set[[i]], j)
#                   dominated_count[[j]] <- dominated_count[[j]] + 1
#
#                 }
#                 if (all(fitness[j, ] <= fitness[i, ]) && any(fitness[j, ] < fitness[i, ])) {
#                   domination_set[[j]] <- c(domination_set[[j]], i)
#                   dominated_count[[i]] <- dominated_count[[i]] + 1
#                 }
#             }
#         }
#
#         if (dominated_count[[i]] == 0) {
#             q <- c(q, i)
#             front[[i]] <- front_index
#             pop_count <- (pop_count + 1)
#         }
#     }
#     f <- list()
#     f[front_index] <- list(sort(q))
#
#     while (TRUE) {
#         q <- c()
#         for (i in f[[front_index]]) {
#             for (j in domination_set[[i]]) {
#                 dominated_count[[j]] <- dominated_count[[j]] - 1
#                 if (dominated_count[j] == 0) {
#                   q <- c(q, j)
#                   front[[j]] <- front_index + 1
#                   pop_count <- pop_count + 1
#                 }
#             }
#         }
#         if (is.null(q))
#             break
#         front_index <- front_index + 1
#         f[front_index] <- list(sort(q))
#     }
#     out <- list(fit = f, fronts = front)
#     return(out)
# }





# cppFunction('
# List non_dominated_fronts_rcpp(NumericMatrix fitness) {
#   int pop_size = fitness.nrow();
#   NumericVector dominated_count(pop_size, 0.0);
#   List domination_set(pop_size);
#   IntegerVector front(pop_size, 0);
#   int front_index = 1;
#
#   for (int i = 0; i < pop_size - 1; ++i) {
#     for (int j = i + 1; j < pop_size; ++j) {
#       bool i_dominates_j = true;
#       bool j_dominates_i = true;
#
#       for (int k = 0; k < fitness.ncol(); ++k) {
#         if (fitness(i, k) > fitness(j, k)) {
#           i_dominates_j = false;
#         }
#         if (fitness(j, k) > fitness(i, k)) {
#           j_dominates_i = false;
#         }
#       }
#
#       if (i_dominates_j && !j_dominates_i) {
#         IntegerVector dom_set = domination_set[i];
#         dom_set.push_back(j);
#         dominated_count[j]++;
#         domination_set[i] = dom_set;
#       } else if (!i_dominates_j && j_dominates_i) {
#         IntegerVector dom_set = domination_set[j];
#         dom_set.push_back(i);
#         dominated_count[i]++;
#         domination_set[j] = dom_set;
#       }
#     }
#   }
#
#   front_index = 1;
#   IntegerVector q = which(dominated_count == 0);
#   for (int i = 0; i < q.size(); ++i) {
#     front[q[i]] = front_index;
#   }
#
#   while (q.size() > 0) {
#     front_index++;
#     IntegerVector new_q;
#     for (int i = 0; i < q.size(); ++i) {
#       IntegerVector dom_set = domination_set[q[i]];
#       for (int j = 0; j < dom_set.size(); ++j) {
#         dominated_count[dom_set[j]]--;
#         if (dominated_count[dom_set[j]] == 0) {
#           new_q.push_back(dom_set[j]);
#           front[dom_set[j]] = front_index;
#         }
#       }
#     }
#     q = new_q;
#   }
#
#   List out;
#   out["fit"] = split(seq_len(pop_size), front);
#   out["fronts"] = front;
#   return out;
# }')
