context("Executing nsga2")

test_that("nsga2() return the value suggestions", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(rep(1,30), ncol = 3)

  out <- nsga2(type = "real-valued", fitness = testfunction,
               lower = c(0,0,0), upper = c(1,1,1),
               popSize = 10, suggestions = x,
               pcrossover = 0, pmutation = 0,
               seed = 1, parallel = FALSE, monitor = FALSE,
               summary = FALSE, maxiter = 1, nObj = 3)

  expect_identical(out@fitness, x)
})
