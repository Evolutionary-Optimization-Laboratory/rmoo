#' Virtual Class "numberOrNAOrMatrix - Simple Class for subassigment Values"
#'
#' The class "numberOrNAOrMatrix" is a simple class union ([setClassUnion()]) of "numeric", "logical" and "matrix".
#'
#' @section Objects from the Class
#' Since it is a virtual Class, no objects may be created from it.
#'
#' @examples
#' showClass("numberOrNAOrMatrix")

#Creamos una nueva clase de la union de numeric, logical y matrix
setClassUnion("numberOrNAOrMatrix", members = c("numeric", "logical", "matrix", "NULL"))


