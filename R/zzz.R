.onLoad <- function(lib, pkg){
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Francisco Jose Benitez Rios",
    devtools.desc.author = 'person("Francisco Jose", "Benitez Rios",
    "benitez.fj@hotmail.com", role = c("aut", "cre"))',
    devtools.desc.license = "Licence",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()
}



options(
  usethis.description = list(
    Title = 'Non-Dominated Genetic Algorithms - III',
    'Authors@R' = 'c(person("Francisco", "Benitez", email = "benitez.fj@hotmail.com", role = c("aut", "cre")),
  person("Diego", "Pinto Roa", email = "dpinto@pol.una.py", role = c("aut"), comment = c(ORCID = "0000-0003-2479-9876")))',
    Description = 'A multi-objective optimization package based on the algorithm of K. Deb and H. Jain.',
    License = "GPL (>= 2)",
    Language = "es"
  )
)


