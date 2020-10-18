context("Executing performance metrics")

test_that("generational_distance() return the value suggestions", {
  x <- generate_reference_points(3,3)

  out <- generational_distance(x,x)

  expect_identical(out, 0)
})
