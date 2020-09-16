nsgaControl <- function(...) {
    current <- .nsga.default
    if (nargs() == 0)
        return(current)
    args <- list(...)
    if (length(args) == 1 && is.null(names(args))) {
        arg <- args[[1]]
        switch(mode(arg),
          list = args <- arg,
          character = return(.nsga.default[[arg]]),
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
        env <- asNamespace("nsga3r") else env <- parent.frame()
    assign(".nsga.default", current, envir = env)
    invisible(current)
}


.nsga.default <- list(binary = list(population = "nsgabin_Population",
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
  eps = sqrt(.Machine$double.eps),
  useRcpp = FALSE)

# .ga.default <- list('binary' = list(population = 'gabin_Population',
#                                       selection = 'gabin_lrSelection',
#                                       crossover = 'gabin_spCrossover',
#                                       mutation = 'gabin_raMutation'),
#                       'real-valued' = list(population = 'gareal_Population',
#                                            selection = 'gareal_lsSelection',
#                                            crossover = 'gareal_laCrossover',
#                                            mutation = 'gareal_raMutation'),
#                       'permutation' = list(population = 'gaperm_Population',
#                                            selection = 'gaperm_lrSelection',
#                                            crossover = 'gaperm_oxCrossover',
#                                            mutation = 'gaperm_simMutation'),
#   'eps' = sqrt(.Machine$double.eps),
#   'useRcpp' = TRUE )
