#' @export
get_fixed_rowsum_integer_matrix <- function(m, h) {
    if (m < 1) {
        stop("M cannot be less than 1.")
    }
    if (floor(m) != m) {
        stop("M must be an integer.")
    }
    if (m == 1) {
        a <- h
        return(a)
    }
    a <- c()
    for (i in 0:h) {
        b <- get_fixed_rowsum_integer_matrix(m - 1, h - i)
        c <- cbind(i, b)
        colnames(c) <- NULL
        a <- rbind(a, c)
        rownames(a) <- NULL
    }
    return(a)
}
