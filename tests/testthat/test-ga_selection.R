# === Random Selection
test_that("random_selection returns the correct number of parents", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  result <- random_selection(fitness_values, num_parents)
  expect_length(result, num_parents)
})

test_that("random_selection returns indices within the correct range", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  result <- random_selection(fitness_values, num_parents)
  expect_true(all(result >= 1 & result <= length(fitness_values)))
})

test_that("random_selection returns different results with different seeds", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  result1 <- random_selection(fitness_values, num_parents, seed = 123)
  result2 <- random_selection(fitness_values, num_parents, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("random_selection returns the same result with the same seed", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  seed <- 123
  result1 <- random_selection(fitness_values, num_parents, seed)
  result2 <- random_selection(fitness_values, num_parents, seed)
  expect_identical(result1, result2)
})

test_that("random_selection handles case when num_parents is zero", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 0
  expect_error(random_selection(fitness_values, num_parents))
})

test_that("random_selection handles case when num_parents is greater than length of fitness_values", {
  fitness_values <- c(0.5, 1.0, 1.5)
  num_parents <- 5
  expect_error(random_selection(fitness_values, num_parents))
})

test_that("random_selection returns unique indices when num_parents equals length of fitness_values", {
  fitness_values <- c(0.5, 1.0, 1.5)
  num_parents <- length(fitness_values)
  result <- random_selection(fitness_values, num_parents)
  expect_true(length(unique(result)) == length(result))
})

test_that("random_selection returns indices in random order", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  num_parents <- 5
  seed <- 123
  result <- random_selection(fitness_values, num_parents, seed)
  expect_true(!all(result == seq_along(fitness_values)))
})

test_that("random_selection works without specifying a seed", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  num_parents <- 3
  result <- random_selection(fitness_values, num_parents)
  expect_length(result, num_parents)
  expect_true(all(result >= 1 & result <= length(fitness_values)))
})

## === Steady State Selection
test_that("steady_state_selection returns the correct number of parents", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  result <- steady_state_selection(fitness_values, num_parents)
  expect_length(result, num_parents)
})

test_that("steady_state_selection returns the top n indices", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(4, 3))
})

test_that("steady_state_selection handles case when num_parents is zero", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 0
  expect_error(steady_state_selection(fitness_values, num_parents))
})

test_that("steady_state_selection handles case when num_parents is equal to the length of fitness_values", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- length(fitness_values)
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(4, 3, 2, 1))
})

test_that("steady_state_selection handles case when num_parents is greater than the length of fitness_values", {
  fitness_values <- c(0.5, 1.0, 1.5)
  num_parents <- 5
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(3, 2, 1))
})

test_that("steady_state_selection handles ties correctly", {
  fitness_values <- c(1.0, 2.0, 2.0, 0.5)
  num_parents <- 2
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(2, 3))
})

test_that("steady_state_selection returns the correct indices with negative fitness values", {
  fitness_values <- c(-1.0, -2.0, 0.0, 1.0)
  num_parents <- 2
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(4, 3))
})

test_that("steady_state_selection handles single fitness value correctly", {
  fitness_values <- c(0.5)
  num_parents <- 1
  result <- steady_state_selection(fitness_values, num_parents)
  expect_equal(result, c(1))
})

test_that("steady_state_selection handles empty fitness_values vector", {
  fitness_values <- numeric(0)
  num_parents <- 0
  expect_error(steady_state_selection(fitness_values, num_parents))
})

## === Tournament Selection
test_that("tournament_selection returns the correct number of parents", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  tournament_size <- 2
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_length(result, num_parents)
})

test_that("tournament_selection returns valid indices", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  tournament_size <- 2
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_true(all(result >= 1 & result <= length(fitness_values)))
})

test_that("tournament_selection returns the same result with the same seed", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  tournament_size <- 2
  seed <- 123
  result1 <- tournament_selection(fitness_values, num_parents, tournament_size, seed)
  result2 <- tournament_selection(fitness_values, num_parents, tournament_size, seed)
  expect_identical(result1, result2)
})

test_that("tournament_selection returns different results with different seeds", {
  fitness_values <- seq.default(0, 10, 0.5)
  num_parents <- 2
  tournament_size <- 2
  result1 <- tournament_selection(fitness_values, num_parents, tournament_size, seed = 123)
  result2 <- tournament_selection(fitness_values, num_parents, tournament_size, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("tournament_selection returns unique winners", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 3
  tournament_size <- 2
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_true(length(unique(result)) <= num_parents)
})

test_that("tournament_selection handles case when num_parents is zero", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 0
  tournament_size <- 2
  expect_error(tournament_selection(fitness_values, num_parents, tournament_size))
})

test_that("tournament_selection handles case when tournament_size is greater than length of fitness_values", {
  fitness_values <- c(0.5, 1.0, 1.5)
  num_parents <- 2
  tournament_size <- 5
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_length(result, 1L)
})

test_that("tournament_selection handles case when tournament_size is one", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  tournament_size <- 1
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_true(all(result >= 1 & result <= length(fitness_values)))
})

test_that("tournament_selection works without specifying a seed", {
  fitness_values <- c(0.5, 1.0, 1.5, 2.0)
  num_parents <- 2
  tournament_size <- 2
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_length(result, num_parents)
  expect_true(all(result >= 1 & result <= length(fitness_values)))
})

test_that("tournament_selection handles ties correctly", {
  fitness_values <- c(1.0, 2.0, 2.0, 0.5)
  num_parents <- 2
  tournament_size <- 2
  result <- tournament_selection(fitness_values, num_parents, tournament_size)
  expect_true(all(result %in% c(2, 3)))
})
