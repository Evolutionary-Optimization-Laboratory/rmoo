#' Determination of Reference Points on a Hyper-Plane
#'
#' A implementation of Das and Dennis's Reference Points Generation.
#'
#' The implemented Reference Point Generation is based on the Das amd Dennis's systematic approach
#' that places points on a normalized hyper-plane which is equally inclined to all objective axes
#' and has an intercept of one on each axis.
#'
#' @param m,h Number of reference points 'h' in M-objective problems
#'
#' @author Francisco Benitez
#'
#' @references K. Deb and H. Jain, 'An Evolutionary Many-Objective Optimization Algorithm Using
#' Reference-Point-Based Nondominated Sorting Approach, Part I: Solving Problems With Box Constraints,'
#' in IEEE Transactions on Evolutionary Computation, vol. 18, no. 4, pp. 577-601, Aug. 2014,
#' doi: 10.1109/TEVC.2013.2281535.
#'
#' @seealso [non_dominated_fronts()] and [get_fixed_rowsum_integer_matrix()]
#'
#' @return A matrix with the reference points uniformly distributed.
generate_reference_points <- function(m, h) {
    zr <- (get_fixed_rowsum_integer_matrix(m, h)/h)
    return(zr)
}
