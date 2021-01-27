.onLoad <- function(lib, pkg) {
    op <- options()
    op.rmoo <- list(rmoo.path = "~/R-dev",
                  rmoo.install.args = "",
                  rmoo.name = "Francisco Jose Benitez Rios",
                  rmoo.desc.author = "person(\"Francisco Jose\", \"Benitez Rios\",
                                             \"benitezfj94@gmail.com\",
                                             role = c(\"aut\", \"cre\"))",
                  rmoo.desc.license = "Licence",
                  rmoo.desc.suggests = NULL, rmoo.desc = list())
    toset <- !(names(op.rmoo) %in% names(op))
    if (any(toset))
        options(op.rmoo[toset])
    invisible()
}

NSGAStartupMessage <- function() {
    msg <- paste0("
       __   _   ____  ____    _
      |   \\ | ||  __|/ ___|  / \\  Non-Dominated
      | |\\ \\| ||__|  | |  _  / _ \\  Genetic
      | | \\ \\ | __| || |_| |/ ___ \\  Algorithms' Family
      |_| \\__||____|\\____/_/   \\_\\  version ",
        packageVersion("rmoo"))
    return(msg)
}

.onAttach <- function(lib, pkg) {
    # unlock .nsga.default variable allowing its modification
    unlockBinding(".nsga.default", asNamespace("rmoo"))
    # startup message
    msg <- NSGAStartupMessage()
    if (!interactive())
        msg[1] <- paste("Package 'rmoo' version", packageVersion("rmoo"))
    packageStartupMessage(msg)
    invisible()
}

options(rmoo.description = list(Title = "R - Multi-Objective Optimization",
          `Authors@R` = "c(person(\"Francisco\", \"Benitez\",
                                email = \"benitezfj94@gmail.com\",
                                role = c(\"aut\", \"cre\")),
                             person(\"Diego\", \"Pinto Roa\",
                                email = \"dpinto@pol.una.py\",
                                role = c(\"aut\"),
                                comment = c(ORCID = \"0000-0003-2479-9876\")))",
        Description = "A multiobjective optimization package based on K. Deb's
    algorithm and inspired in 'GA' package by Luca Scrucca (2017) <DOI:10.32614/RJ-2017-008>.
  The 'rmoo' package is a framework for multi- and many-objective optimization,
  allowing to work with representation of real numbers, permutations and binaries,
  offering a high range of configurations.",
        License = "GPL (>= 2)",
        Language = "es"))
