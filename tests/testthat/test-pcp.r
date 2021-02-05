context("test-pcp-.r")

test_that("Parallel Coordinate Plot for n-objective works", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(runif(40, 0, 1), ncol = 4, nrow = 10)

  out <- nsga3(type = "real-valued", fitness = testfunction,
    lower = rep(0,4), upper = rep(1,4),
    popSize = 10, suggestions = x,
    n_partitions = 2,
    pcrossover = 0.1, pmutation = 0.1,
    seed = 1, monitor = FALSE,
    summary = FALSE, maxiter = 1, nObj = 4)

  expect_message(pcp(object = out), NA)
  expect_message(heat_map(fitness = out@fitness), NA)
  expect_message(polar(fitness = out@fitness[1:3,]), NA)
})
