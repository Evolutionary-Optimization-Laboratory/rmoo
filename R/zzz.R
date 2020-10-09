.onLoad <- function(lib, pkg) {
    op <- options()
    op.nsga3r <- list(nsga3r.path = "~/R-dev",
                      nsga3r.install.args = "",
                      nsga3r.name = "Francisco Jose Benitez Rios",
                      nsga3r.desc.author = "person(\"Francisco Jose\", \"Benitez Rios\",
                                                        \"benitez.fj@hotmail.com\",
                                                         role = c(\"aut\", \"cre\"))",
                      nsga3r.desc.license = "Licence",
                      nsga3r.desc.suggests = NULL, nsga3r.desc = list())
    toset <- !(names(op.nsga3r) %in% names(op))
    if (any(toset))
        options(op.nsga3r[toset])
    invisible()
}

NSGAStartupMessage <- function() {
    msg <- paste0("
       __   _   ____  ____    _
      |   \\ | ||  __|/ ___|  / \\  Non-Dominated
      | |\\ \\| ||__|  | |  _  / _ \\  Genetic
      | | \\ \\ | __| || |_| |/ ___ \\  Algorithms-III
      |_| \\__||____|\\____/_/   \\_\\  version ",
        packageVersion("nsga3r"))
    return(msg)
}

.onAttach <- function(lib, pkg) {
    # unlock .nsga.default variable allowing its modification
    unlockBinding(".nsga.default", asNamespace("nsga3r"))
    # startup message
    msg <- NSGAStartupMessage()
    if (!interactive())
        msg[1] <- paste("Package 'nsga3r' version", packageVersion("nsga3r"))
    packageStartupMessage(msg)
    invisible()
}

options(nsga3r.description = list(Title = "Family of Non-Dominated Genetic Algorithms",
                                  `Authors@R` = "c(person(\"Francisco\", \"Benitez\",
                                                      email = \"benitez.fj@hotmail.com\",
                                                      role = c(\"aut\", \"cre\")),
                                                   person(\"Diego\", \"Pinto Roa\",
                                                      email = \"dpinto@pol.una.py\",
                                                      role = c(\"aut\"),
                                                      comment = c(ORCID = \"0000-0003-2479-9876\")))",
        Description = "A multiobjective optimization package based on K. Deb's
    algorithm and inspired by Luca Scrucca's GA package <10.32614/RJ-2017-008>.
  The nsga3r package is a framework for multi- and many-objective optimization,
  allowing to work with representation of real numbers, permutations and binaries,
  offering a high range of configurations.",
        License = "GPL (>= 2)",
        Language = "es"))
