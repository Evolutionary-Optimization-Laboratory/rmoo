#' Virtual Class 'OptNumberOrMatrix - Simple Class for subassigment Values'
#'
#' The class 'OptNumberOrMatrix' is a simple class union ([setClassUnion()])
#' of 'numeric', 'logical' and 'matrix'.
#'
#' @section Objects from the Class:
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @examples
#' showClass('OptNumberOrMatrix')

# A new class is created with the union of numeric, logical and matrix
#' @export
setClassUnion("numberOrMatrix", c("numeric", "logical", "matrix", "NULL"))

#' @export
setClassUnion("OptNumberOrMatrix", c("numberOrMatrix", "NULL"))
