context("Executing nsga")

test_that("nsga() return the value suggestions", {
  testfunction <- function (x){
    return(x)
  }

  x <- matrix(rep(1,30), ncol = 3)

  out <- nsga(type = "real-valued", fitness = testfunction,
              lower = c(0,0,0), upper = c(1,1,1),
              popSize = 10, suggestions = x,
              pcrossover = 0, pmutation = 0,
              seed = 1, dshare = 1, monitor = FALSE,
              maxiter = 1, nObj = 3)

  expect_identical(out@fitness, x)
})
