context("test-scatter-.r")

test_that("Scatter Plot for 2-objective works", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(runif(20, 0, 1), ncol = 2, nrow = 10)

  out <- rmoo(type = "real-valued", algorithm = "NSGA-II", fitness = testfunction,
              lower = c(0,0), upper = c(1,1),
              popSize = 10, suggestions = x,
              pcrossover = 0, pmutation = 0,
              seed = 1, parallel = FALSE, monitor = FALSE,
              summary = FALSE, maxiter = 1, nObj = 2)

  expect_message(plot(x = out), NA)
})

test_that("Scatter Plot for 3-objective works", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(runif(30, 0, 1), ncol = 3, nrow = 10)
  ref_points <- generate_reference_points(3,3)

  out <- rmoo(type = "real-valued", algorithm = "NSGA-III", fitness = testfunction,
	      lower = c(0,0,0), upper = c(1,1,1),
	      popSize = 10, suggestions = x,
	      reference_dirs = ref_points,
	      pcrossover = 0.1, pmutation = 0.1,
	      seed = 1, parallel = FALSE, monitor = FALSE,
	      summary = FALSE, maxiter = 1, nObj = 3)

  expect_message(plot(x = out), NA)
  expect_message(plot(x = out,
                 optimal = out@reference_points), NA)
})

test_that("Scatter Plot for n-objective works", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(runif(40, 0, 1), ncol = 4, nrow = 10)
  ref_points <- generate_reference_points(4,2)

  out <- rmoo(type = "real-valued", algorithm = "NSGA-III", fitness = testfunction,
    lower = rep(0,4), upper = rep(1,4),
    popSize = 10, suggestions = x,
    reference_dirs = ref_points,
    pcrossover = 0.1, pmutation = 0.1,
    seed = 1, parallel = FALSE, monitor = FALSE,
    summary = FALSE, maxiter = 1, nObj = 4)

  expect_message(plot(x = out), NA)
})
