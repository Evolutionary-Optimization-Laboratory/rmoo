context("Running rmoo check_validation")

test_that("check_numeric_arg() return the value suggestions", {
  expect_error(check_numeric_arg(arg=NULL, "test input", check_negative = FALSE))
  expect_error(check_numeric_arg(arg="3", "test input", check_negative = FALSE))
  expect_error(check_numeric_arg(arg=3.5, "test input", check_negative = FALSE))
  expect_error(check_numeric_arg(arg=-3, "test input", check_negative = FALSE))
})


test_that("check_probability_arg() return the value suggestions", {
  expect_error(check_probability_arg(arg="0.1", "test input"))
  expect_error(check_probability_arg(arg=-0.1, "test input"))
  expect_error(check_probability_arg(arg=1.1, "test input"))
  # expect_identical(colSums(out), 0)
})

test_that("check_function_arg() return the value suggestions", {
  expect_error(check_function_arg(arg=NULL, "test input"))
  expect_error(check_function_arg(arg=3, "test input"))
})

test_that("check_matrix_arg() return the value suggestions", {
  expect_error(check_matrix_arg(arg=NULL, "test input"))
  expect_error(check_matrix_arg(arg=3, "test input"))
  expect_error(check_matrix_arg(arg="3", "test input"))
})

test_that("check_algorithm_arg() return the value suggestions", {
  expect_error(check_algorithm_arg(nObj=3, algorithm="R-NSGA-II"))
  expect_error(check_algorithm_arg(nObj=3, algorithm="NSGA-III"))
})
