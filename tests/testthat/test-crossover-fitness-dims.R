context("Crossover fitness matrix dimensions")

# Helper to create a minimal nsga2 object for testing crossover functions
create_test_object <- function(type = "real-valued", nObj = 2) {
  if (type == "real-valued") {
    pop <- matrix(runif(30), nrow = 10, ncol = 3)
  } else {
    pop <- matrix(sample(0:1, 30, replace = TRUE), nrow = 10, ncol = 3)
    storage.mode(pop) <- "integer"
  }

  new("nsga2",
      call = call("test"),
      type = type,
      lower = c(0, 0, 0),
      upper = c(1, 1, 1),
      nBits = NA_integer_,
      names = c("x1", "x2", "x3"),
      popSize = 10L,
      front = matrix(1L, nrow = 10, ncol = 1),
      f = list(),
      iter = 1L,
      run = 1L,
      maxiter = 10L,
      suggestions = matrix(nrow = 0, ncol = 3),
      population = pop,
      pcrossover = 0.8,
      pmutation = 0.1,
      crowdingDistance = matrix(runif(10), nrow = 10),
      fitness = matrix(runif(10 * nObj), nrow = 10, ncol = nObj),
      summary = list())
}

test_that("rmooreal_sbxCrossover returns 2-row fitness matrix", {
  object <- create_test_object("real-valued", nObj = 2)
  parents <- c(1L, 2L)

  result <- rmooreal_sbxCrossover(object, parents)

  expect_equal(nrow(result$children), 2)
  expect_equal(nrow(result$fitness), 2)
  expect_equal(ncol(result$fitness), 2)
})

test_that("rmoo_spCrossover returns 2-row fitness matrix", {
  object <- create_test_object("real-valued", nObj = 3)
  parents <- c(1L, 2L)

  # Run multiple times to hit different branches (crossOverPoint = 0, n, or middle)
  set.seed(123)
  for (i in 1:10) {
    result <- rmoo_spCrossover(object, parents)

    expect_equal(nrow(result$children), 2)
    expect_true(is.matrix(result$fitness),
                info = "fitness should be a matrix, not a vector")
    expect_equal(nrow(result$fitness), 2)
    expect_equal(ncol(result$fitness), 3)
  }
})

test_that("rmoo_uxCrossover returns 2-row fitness matrix", {
  object <- create_test_object("binary", nObj = 2)
  parents <- c(1L, 2L)

  result <- rmoo_uxCrossover(object, parents)

  expect_equal(nrow(result$children), 2)
  expect_equal(nrow(result$fitness), 2)
  expect_equal(ncol(result$fitness), 2)
})

test_that("rmoo_huxCrossover returns 2-row fitness matrix", {
  object <- create_test_object("binary", nObj = 2)
  parents <- c(1L, 2L)

  result <- rmoo_huxCrossover(object, parents)

  expect_equal(nrow(result$children), 2)
  expect_equal(nrow(result$fitness), 2)
  expect_equal(ncol(result$fitness), 2)
})

test_that("crossover fitness can be assigned to 2 parent rows without recycling", {
  # This test verifies the fix works in the actual algorithm context
  object <- create_test_object("real-valued", nObj = 2)
  parents <- c(1L, 2L)

  result <- rmooreal_sbxCrossover(object, parents)

  # Simulate nsga2.R line 401: Fitness[parents, ] <- Crossover$fitness
  Fitness <- matrix(1:20, nrow = 10, ncol = 2)
  original_row3 <- Fitness[3, ]

  # This should work without R recycling a 1-row matrix
  Fitness[parents, ] <- result$fitness

  # Verify row 3 wasn't affected (would be if recycling occurred incorrectly)
  expect_equal(Fitness[3, ], original_row3)

  # Verify both parent rows were updated
  expect_true(all(is.na(Fitness[1, ])))
  expect_true(all(is.na(Fitness[2, ])))
})


test_that("rmoo_tpCrossover returns 2-row fitness matrix", {
  # tpCrossover works on binary/integer populations; it swaps a segment between
  # two parents, so both rows of the fitness output must be NA placeholders.
  object  <- create_test_object("binary", nObj = 2)
  parents <- c(1L, 2L)

  result <- rmoo_tpCrossover(object, parents)

  expect_true(is.matrix(result$fitness),
              info = "fitness must be a matrix, not a vector")
  expect_equal(nrow(result$fitness), 2,
               info = "tpCrossover produces 2 children, so fitness needs 2 rows")
  expect_equal(ncol(result$fitness), 2)
  expect_equal(nrow(result$children), 2)
})

test_that("pointCrossover returns 2-row fitness matrix", {
  # pointCrossover is the generalised n-point version; it always produces
  # exactly 2 children regardless of n_points.
  object  <- create_test_object("binary", nObj = 2)
  parents <- c(1L, 2L)

  result <- pointCrossover(object, parents, n_points = 2)

  expect_true(is.matrix(result$fitness),
              info = "fitness must be a matrix, not a vector")
  expect_equal(nrow(result$fitness), 2,
               info = "pointCrossover produces 2 children, so fitness needs 2 rows")
  expect_equal(ncol(result$fitness), 2)
  expect_equal(nrow(result$children), 2)
})

test_that("rmooperm_oxCrossover returns 2-row fitness matrix", {
  # oxCrossover operates on permutation representations; its fitness output
  # was a bare rep(NA, 2) vector — a bug missed by the PR.
  # ncol >= 5 is required so cxPoints can always find 2 interior positions.
  set.seed(42)
  pop <- t(replicate(10, sample(1:6)))        # 10 × 6 permutation matrix

  object <- new("nsga2",
                call     = call("test"),
                type     = "permutation",
                lower    = 1L,
                upper    = 6L,
                nBits    = NA_integer_,
                names    = as.character(1:6),
                popSize  = 10L,
                front    = matrix(1L, nrow = 10, ncol = 1),
                f        = list(),
                iter     = 1L,
                run      = 1L,
                maxiter  = 10L,
                suggestions      = matrix(nrow = 0, ncol = 6),
                population       = pop,
                pcrossover       = 0.8,
                pmutation        = 0.1,
                crowdingDistance = matrix(runif(10), nrow = 10),
                fitness          = matrix(runif(20), nrow = 10, ncol = 2),
                summary          = list())
  parents <- c(1L, 2L)

  result <- rmooperm_oxCrossover(object, parents)

  expect_true(is.matrix(result$fitness),
              info = "fitness must be a matrix, not a bare rep(NA, 2) vector")
  expect_equal(nrow(result$fitness), 2,
               info = "oxCrossover produces 2 children, so fitness needs 2 rows")
  expect_equal(ncol(result$fitness), 2)
  expect_equal(nrow(result$children), 2)
})
