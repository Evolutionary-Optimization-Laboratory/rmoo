context("Executing nsga3")

test_that("nsga3() return the value suggestions", {
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

  expect_identical(dim(out@fitness), dim(x))
})
