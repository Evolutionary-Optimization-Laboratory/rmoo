context("test-pcp-.r")

test_that("Parallel Coordinate Plot for n-objective works", {
  testfunction <- function (x,...){
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

  expect_message(plot(x = out, type = "pcp"), NA)
  expect_message(plot(x = out, type = "heatmap", individual = c(1:5)), NA)
  expect_message(plot(x = out, type = "polar", individual = c(1:5)), NA)
})
