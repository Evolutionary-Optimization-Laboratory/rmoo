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

rmooStartupMessage <- function() {
    msg <- paste0("
       ____  __    __  _____  _____
      |  _ | | \\  /  ||  _  ||  _  | R
      |  __| | |\\/ / || | | || | | | Multi-/Many-
      | |\\\\  | | \\/| || |_| || |_| | Objective
      |_| \\\\ |_|   |_||_____||_____| Optimization Package ",
        packageVersion("rmoo"))
    return(msg)
}

.onAttach <- function(lib, pkg) {
    # unlock .rmoo.default variable allowing its modification
    unlockBinding(".rmoo.default", asNamespace("rmoo"))
    # startup message
    msg <- rmooStartupMessage()
    if (!interactive())
        msg[1] <- paste("Package 'rmoo' version", packageVersion("rmoo"))
    packageStartupMessage(msg)
    invisible()
}

options(rmoo.description = list(Title = "Multi-Objective Optimization in R",
          `Authors@R` = "c(person(\"Francisco\", \"Benitez\",
                                email = \"benitezfj94@gmail.com\",
                                role = c(\"aut\", \"cre\")),
                             person(\"Diego P.\", \"Pinto-Roa\",
                                email = \"dpinto@pol.una.py\",
                                role = c(\"aut\"),
                                comment = c(ORCID = \"0000-0003-2479-9876\")))",
        Description = "The 'rmoo' package is a framework for omulti- and many-
        objective optimization, which allows researchers and users versatility
        in parameter configuration, as well as tools for analysis, replication
        and visualization of results. The 'rmoo' package was built as a fork of
        the 'GA' package by Luca Scrucca(2017) <DOI:10.32614/RJ-2017-008> and
        implementing the Non-Dominated Sorting Genetic Algorithms proposed
        by K. Deb's.",
        License = "GPL (>= 2)",
        Language = "es"))
