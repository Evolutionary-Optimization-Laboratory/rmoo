
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



# front and true_pareto_front can be:
#   N×m matrix where N is the number of points and m is the number of objectives.
#   State
#   Array{xFgh_indiv} (usually `State.population`)
# true_pareto_front: is a M×m matrix.
# p_ is the power in for the ‖⋅‖_p
# plus: if true then computes the GD+
# inverted: if true then computes IGD

#' @export
generational_distance <- function(front, true_pareto_front, p = 1, inverted = FALSE, plus = FALSE) {
  if (inverted) {
    tmp <- true_pareto_front
    true_pareto_front <- front
    front <- tmp
  }

  distances <- rep(0, nrow(true_pareto_front))

  s <- 0.0
  for (i in 1:nrow(front)) {
    x <- front[i,]
    for (j in 1:nrow(true_pareto_front)) {
      y <- true_pareto_front[j,]
      # IGD Plus
      if (plus && inverted) {
        distances[j] <- norm(pmax(y - x, 0), type = "2")
        # GD Plus
      } else if (plus && !inverted) {
        distances[j] <- norm(pmax(x - y, 0), type = "2")
        # GD and IGD
      } else {
        distances[j] <- norm(y - x, type = "2")
      }
    }

    s <- s + min(distances)^p
  }

  return ((s^(1/p)) / nrow(front))
}

gd_plus <- function(front, true_pareto_front, p = 1) {
  generational_distance(front, true_pareto_front, p = p, plus = TRUE)
}
# generational_distance_cpp <- cppFunction("
#   NumericVector generational_distance_cpp(NumericMatrix front, NumericMatrix true_pareto_front,
#                                           double p = 1, bool inverted = false, bool plus = false) {
#     int n_front = front.nrow();
#     int n_true_pareto = true_pareto_front.nrow();
#     NumericVector distances(n_true_pareto);
#     double s = 0.0;
#
#     for (int i = 0; i < n_front; ++i) {
#       NumericVector x = front(i, _);
#       for (int j = 0; j < n_true_pareto; ++j) {
#         NumericVector y = true_pareto_front(j, _);
#         if (plus && inverted) {
#           for (int k = 0; k < x.length(); ++k) {
#             distances[j] += pow(std::max(y[k] - x[k], 0.0), 2);
#           }
#         } else if (plus && !inverted) {
#           for (int k = 0; k < x.length(); ++k) {
#             distances[j] += pow(std::max(x[k] - y[k], 0.0), 2);
#           }
#         } else {
#           for (int k = 0; k < x.length(); ++k) {
#             distances[j] += pow(y[k] - x[k], 2);
#           }
#         }
#       }
#       s += pow(*std::min_element(distances.begin(), distances.end()), p);
#     }
#
#     return pow(s, 1/p) / n_front;
#   }
# ")
#
# gd_plus_cpp <- function(front, true_pareto_front, p = 1) {
#   generational_distance_cpp(front, true_pareto_front, p = p, plus = TRUE)
# }



# #include <Rcpp.h>
# using namespace Rcpp;
#
# // [[Rcpp::export]]
# double generational_distance(NumericMatrix front, NumericMatrix true_pareto_front,
#                              double p = 1, bool inverted = false, bool plus = false) {
#   if (inverted) {
#     NumericMatrix tmp = true_pareto_front;
#     true_pareto_front = front;
#     front = tmp;
#   }
#
#   int nrow_true_pareto_front = true_pareto_front.nrow();
#   NumericVector distances(nrow_true_pareto_front);
#
#   double s = 0.0;
#   for (int i = 0; i < front.nrow(); i++) {
#     NumericVector x = front(i, _);
#     for (int j = 0; j < nrow_true_pareto_front; j++) {
#       NumericVector y = true_pareto_front(j, _);
#       // IGD Plus
#       if (plus && inverted) {
#         distances[j] = sqrt(sum(pow(pmax(y - x, 0), 2)));
#         // GD Plus
#       } else if (plus && !inverted) {
#         distances[j] = sqrt(sum(pow(pmax(x - y, 0), 2)));
#         // GD and IGD
#       } else {
#         distances[j] = sqrt(sum(pow(y - x, 2)));
#       }
#     }
#
#     s += pow(min(distances), p);
#   }
#
#   return pow(s, 1 / p) / front.nrow();
# }
#
# // [[Rcpp::export]]
# double gd_plus(NumericMatrix front, NumericMatrix true_pareto_front, double p = 1) {
#   return generational_distance(front, true_pareto_front, p, false, true);
# }
#
# library(Rcpp)
# sourceCpp("path/to/cpp/code.cpp")
