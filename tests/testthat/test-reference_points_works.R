context("rmoo-function")
test_that("reference points works", {
  points <- generate_reference_points(1,1)
  expect_identical(points, 1)
})
