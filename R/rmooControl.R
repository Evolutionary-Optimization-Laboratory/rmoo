#' @export
rmooControl <- function(...) {
    current <- .rmoo.default
    if (nargs() == 0)
        return(current)
    args <- list(...)
    if (length(args) == 1 && is.null(names(args))) {
        arg <- args[[1]]
        switch(mode(arg),
          list = args <- arg,
          character = return(.rmoo.default[[arg]]),
          stop("invalid argument: ", dQuote(arg))
        )
    }

    if (length(args) == 0)
        return(current)
    nargs <- names(args)
    if (is.null(nargs))
        stop("options must be given by name")

    if (is.list(args)) {
        changed <- current[nargs]
        for (i in 1:length(nargs)) {
            if (is.list(args[[i]])) {
                what <- names(args[[i]])
                changed[[i]][what] <- args[[i]][what]
            } else {
                changed[i] <- args[[i]]
            }
        }
        current[nargs] <- changed
    } else {
        changed <- current[nargs]
        current[nargs] <- args
    }

    if (sys.parent() == 0)
        env <- asNamespace("rmoo") else env <- parent.frame()
    assign(".rmoo.default", current, envir = env)
    invisible(current)
}


.rmoo.default <- list(binary = list(population = "rmoobin_Population",
                                    selection = "rmoobin_tourSelection",
                                    crossover = "rmoobin_spCrossover",
                                    mutation = "rmoobin_raMutation"),
                     `real-valued` = list(population = "rmooreal_Population",
                                          selection = "rmooreal_tourSelection",
                                          crossover = "rmooreal_sbxCrossover",
                                          mutation = "rmooreal_polMutation"),
                     permutation = list(population = "rmooperm_Population",
                                        selection = "rmooperm_tourSelection",
                                        crossover = "rmooperm_oxCrossover",
                                        mutation = "rmooperm_simMutation"),
                     discrete = list(population = "rmooint_Population",
                                        selection = "rmooint_tourSelection",
                                        crossover = "rmooint_uxCrossover",
                                        mutation = "rmooint_uxMutation"),
  eps = sqrt(.Machine$double.eps),
  useRcpp = FALSE)
