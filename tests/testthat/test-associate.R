context("Executing compute_perpendicular_distance")

test_that("compute_perpendicular_distance() return the value suggestions", {
  x <- matrix(rep(3,30), nrow = 10)
  y <- matrix(rep(1,3), nrow = 1)

  out <- compute_perpendicular_distance(x,y)

  expect_identical(colSums(out), 0)
})
