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

options(nsga3r.description = list(Title = "Non-Dominated Genetic Algorithms - III",
                                  `Authors@R` = "c(person(\"Francisco\", \"Benitez\",
                                                      email = \"benitez.fj@hotmail.com\",
                                                      role = c(\"aut\", \"cre\")),
                                                   person(\"Diego\", \"Pinto Roa\",
                                                      email = \"dpinto@pol.una.py\",
                                                      role = c(\"aut\"),
                                                      comment = c(ORCID = \"0000-0003-2479-9876\")))",
        Description = "A multi-objective optimization package based on the algorithm of K. Deb and H. Jain.",
        License = "GPL (>= 2)",
        Language = "es"))
