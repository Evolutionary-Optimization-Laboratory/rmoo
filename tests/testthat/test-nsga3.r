context("Executing nsga3")

test_that("nsga3() return the value suggestions", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(runif(30, 0, 1), ncol = 3, nrow = 10)

  out <- nsga3(type = "real-valued", fitness = testfunction,
               lower = c(0,0,0), upper = c(1,1,1),
               popSize = 10, suggestions = x,
               n_partitions = 3,
               pcrossover = 0.1, pmutation = 0.1,
               seed = 1, monitor = FALSE,
               summary = FALSE, maxiter = 1, nObj = 3)

  expect_identical(dim(out@fitness), dim(x))
})
