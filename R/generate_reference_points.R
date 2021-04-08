#' Determination of Reference Points on a Hyper-Plane
#'
#' A implementation of Das and Dennis's Reference Points Generation.
#'
#' The implemented Reference Point Generation is based on the Das and Dennis's
#' systematic approach that places points on a normalized hyper-plane which is
#' equally inclined to all objective axes and has an intercept of one on each axis.
#'
#' @param m,h,scaling Number of reference points 'h' in M-objective problems, and
#' scaling that is the scale on which the points are distributed.
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
#' Das, Indraneel & Dennis, J. (2000). Normal-Boundary Intersection: A New
#' Method for Generating the Pareto Surface in Nonlinear Multicriteria
#' Optimization Problems. SIAM Journal on Optimization. 8.
#' 10.1137/S1052623496307510.
#'
#' @seealso [non_dominated_fronts()] and [get_fixed_rowsum_integer_matrix()]
#'
#' @return A matrix with the reference points uniformly distributed.
#' @export
generate_reference_points <- function(m, h, scaling = NULL) {
    zr <- (get_fixed_rowsum_integer_matrix(m, h) / h)
    if (!is.null(scaling)) zr <- scale_reference_directions(zr ,scaling)
    return(zr)
}

#' Scale Reference Points
#'
#' A implementation of Das and Dennis's Reference Points Generation.
#'
#' The implemented Reference Point Generation is based on the Das and Dennis's
#' systematic approach that places points on a normalized hyper-plane which is
#' equally inclined to all objective axes and has an intercept of one on each axis.
#'
#' @param ref_dirs,scaling where 'ref_dirs' are the reference points generated and
#' 'scaling' are the scale on which the points are distributed.
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references J. Blank and K. Deb, "Pymoo: Multi-Objective Optimization in Python," in
#' IEEE Access, vol. 8, pp. 89497-89509, 2020, doi: 10.1109/ACCESS.2020.2990567.
#'
#' @seealso [generate_reference_points()] and [get_fixed_rowsum_integer_matrix()]
#'
#' @return A matrix with rescaled reference points uniformly distributed.
#' @export
scale_reference_directions <- function(ref_dirs, scaling){
  return(ref_dirs * scaling + ((1 - scaling) / ncol(ref_dirs)))
}
