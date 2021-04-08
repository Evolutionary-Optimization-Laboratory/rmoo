#' Determination of Multi-layer Reference Points
#'
#' A implementation of Multi-layer Reference Points Generation.
#'
#' The Multi-layer reference point implementation is based on Blank and Deb's
#' pymoo library, the approach generates different layers of references point
#' at different scales, provided by the user.
#'
#' @param ... The different layers provided by the user
#'
#' @author Francisco Benitez
#' \email{benitezfj94@gmail.com}
#'
#' @references J. Blank and K. Deb, "Pymoo: Multi-Objective Optimization in
#' Python," in IEEE Access, vol. 8, pp. 89497-89509, 2020,
#' doi: 10.1109/ACCESS.2020.2990567.
#'
#' Das, Indraneel & Dennis, J. (2000). Normal-Boundary Intersection: A New
#' Method for Generating the Pareto Surface in Nonlinear Multicriteria
#' Optimization Problems. SIAM Journal on Optimization. 8.
#' 10.1137/S1052623496307510.
#'
#' @seealso [generate_reference_points()] and [get_fixed_rowsum_integer_matrix()]
#'
#' @return A matrix with the multi-layer reference points
#' @export
reference_point_multi_layer <- function(...){
  callArgs <- list(...)
  if (var(sapply(callArgs, ncol)) != 0){
    stop("All the input array dimensions must match exactly for the column axis.")
  }
  multi_number <- do.call(rbind, callArgs)
  multi_number <- round(multi_number, digits = 8)
  multi_number <- multi_number[!duplicated(multi_number),]

  return(multi_number)
}
