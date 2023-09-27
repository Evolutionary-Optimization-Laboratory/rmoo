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


.rmoo.default <- list(binary = list(population = "nsgabin_Population",
                                    selection = "nsgabin_tourSelection",
                                    crossover = "nsgabin_spCrossover",
                                    mutation = "nsgabin_raMutation"),
                     `real-valued` = list(population = "nsgareal_Population",
                                          selection = "nsgareal_tourSelection",
                                          crossover = "nsgareal_sbxCrossover",
                                          mutation = "nsgareal_polMutation"),
                     permutation = list(population = "nsgaperm_Population",
                                        selection = "nsgaperm_tourSelection",
                                        crossover = "nsgaperm_oxCrossover",
                                        mutation = "nsgaperm_simMutation"),
                     discrete = list(population = "nsgaint_Population",
                                        selection = "nsgaint_tourSelection",
                                        crossover = "nsgaint_uxCrossover",
                                        mutation = "nsgaint_uxMutation"),
  eps = sqrt(.Machine$double.eps),
  useRcpp = FALSE)
