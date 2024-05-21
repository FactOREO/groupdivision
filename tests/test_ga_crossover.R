library("testthat")
source("../R/ga_crossover.R")

## === Single Point Crossover
test_that("single_point_crossover generates two children with correct lengths", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result <- single_point_crossover(parent1, parent2, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("single_point_crossover returns the same result with the same seed", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result1 <- single_point_crossover(parent1, parent2, seed)
  result2 <- single_point_crossover(parent1, parent2, seed)
  expect_identical(result1, result2)
})

test_that("single_point_crossover returns different results with different seeds", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  result1 <- single_point_crossover(parent1, parent2, seed = 123)
  result2 <- single_point_crossover(parent1, parent2, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("single_point_crossover handles empty parents", {
  parent1 <- numeric(0)
  parent2 <- numeric(0)
  expect_error(single_point_crossover(parent1, parent2))
})

test_that("single_point_crossover handles single element parents", {
  parent1 <- c(1)
  parent2 <- c(2)
  result <- single_point_crossover(parent1, parent2, seed = 123)
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("single_point_crossover handles two element parents", {
  parent1 <- c(1, 2)
  parent2 <- c(3, 4)
  result <- single_point_crossover(parent1, parent2, seed = 123)
  expect_length(result[[1]], 2)
  expect_length(result[[2]], 2)
})

test_that("single_point_crossover correctly crosses over when indices are at the edges", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 456
  result <- single_point_crossover(parent1, parent2, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("single_point_crossover produces valid children", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result <- single_point_crossover(parent1, parent2, seed)
  expect_true(all(result[[1]] %in% c(parent1, parent2)))
  expect_true(all(result[[2]] %in% c(parent1, parent2)))
})

## === Multi Point Crossover
test_that("multi_point_crossover generates two children with correct lengths", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  k <- 2
  seed <- 123
  result <- multi_point_crossover(parent1, parent2, k, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("multi_point_crossover returns the same result with the same seed", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  k <- 2
  seed <- 123
  result1 <- multi_point_crossover(parent1, parent2, k, seed)
  result2 <- multi_point_crossover(parent1, parent2, k, seed)
  expect_identical(result1, result2)
})

test_that("multi_point_crossover returns different results with different seeds", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  k <- 2
  result1 <- multi_point_crossover(parent1, parent2, k, seed = 123)
  result2 <- multi_point_crossover(parent1, parent2, k, seed = 124)
  expect_false(identical(result1, result2))
})

test_that("multi_point_crossover handles empty parents", {
  parent1 <- numeric(0)
  parent2 <- numeric(0)
  k <- 2
  expect_error(multi_point_crossover(parent1, parent2, k))
})

test_that("multi_point_crossover handles single element parents", {
  parent1 <- c(1)
  parent2 <- c(2)
  k <- 2
  result <- multi_point_crossover(parent1, parent2, k, seed = 123)
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("multi_point_crossover handles two element parents", {
  parent1 <- c(1, 2)
  parent2 <- c(3, 4)
  k <- 2
  result <- multi_point_crossover(parent1, parent2, k, seed = 123)
  expect_length(result[[1]], 2)
  expect_length(result[[2]], 2)
})

test_that("multi_point_crossover correctly crosses over when k is greater than the length of the parents", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  k <- 6
  seed <- 456
  result <- multi_point_crossover(parent1, parent2, k, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("multi_point_crossover produces valid children", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  k <- 2
  seed <- 123
  result <- multi_point_crossover(parent1, parent2, k, seed)
  expect_true(all(result[[1]] %in% c(parent1, parent2)))
  expect_true(all(result[[2]] %in% c(parent1, parent2)))
})

## === Shuffled Crossover
test_that("shuffled_crossover generates two children with correct lengths", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result <- shuffled_crossover(parent1, parent2, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("shuffled_crossover returns the same result with the same seed", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result1 <- shuffled_crossover(parent1, parent2, seed)
  result2 <- shuffled_crossover(parent1, parent2, seed)
  expect_identical(result1, result2)
})

test_that("shuffled_crossover returns different results with different seeds", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  result1 <- shuffled_crossover(parent1, parent2, seed = 123)
  result2 <- shuffled_crossover(parent1, parent2, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("shuffled_crossover handles empty parents", {
  parent1 <- numeric(0)
  parent2 <- numeric(0)
  expect_error(shuffled_crossover(parent1, parent2))
})

test_that("shuffled_crossover handles single element parents", {
  parent1 <- c(1)
  parent2 <- c(2)
  result <- shuffled_crossover(parent1, parent2, seed = 123)
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("shuffled_crossover handles two element parents", {
  parent1 <- c(1, 2)
  parent2 <- c(3, 4)
  result <- shuffled_crossover(parent1, parent2, seed = 123)
  expect_length(result[[1]], 2)
  expect_length(result[[2]], 2)
})

test_that("shuffled_crossover correctly crosses over shuffled parents", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  seed <- 123
  result <- shuffled_crossover(parent1, parent2, seed)
  expect_true(all(result[[1]] %in% c(parent1, parent2)))
  expect_true(all(result[[2]] %in% c(parent1, parent2)))
})

## === Uniform Crossover
test_that("uniform_crossover generates two children with correct lengths", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  probability <- 0.5
  seed <- 123
  result <- uniform_crossover(parent1, parent2, probability, seed)
  expect_length(result[[1]], length(parent1))
  expect_length(result[[2]], length(parent2))
})

test_that("uniform_crossover returns the same result with the same seed", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  probability <- 0.5
  seed <- 123
  result1 <- uniform_crossover(parent1, parent2, probability, seed)
  result2 <- uniform_crossover(parent1, parent2, probability, seed)
  expect_identical(result1, result2)
})

test_that("uniform_crossover returns different results with different seeds", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  probability <- 0.5
  result1 <- uniform_crossover(parent1, parent2, probability, seed = 123)
  result2 <- uniform_crossover(parent1, parent2, probability, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("uniform_crossover handles empty parents", {
  parent1 <- numeric(0)
  parent2 <- numeric(0)
  expect_error(uniform_crossover(parent1, parent2))
})

test_that("uniform_crossover handles single element parents", {
  parent1 <- c(1)
  parent2 <- c(2)
  probability <- 0.5
  result <- uniform_crossover(parent1, parent2, probability, seed = 123)
  expect_length(result[[1]], 1)
  expect_length(result[[2]], 1)
})

test_that("uniform_crossover handles two element parents", {
  parent1 <- c(1, 2)
  parent2 <- c(3, 4)
  probability <- 0.5
  result <- uniform_crossover(parent1, parent2, probability, seed = 123)
  expect_length(result[[1]], 2)
  expect_length(result[[2]], 2)
})

test_that("uniform_crossover correctly performs crossover based on probability", {
  parent1 <- c(1, 2, 3, 4, 5)
  parent2 <- c(6, 7, 8, 9, 10)
  probability <- 0.3
  seed <- 123
  result <- uniform_crossover(parent1, parent2, probability, seed)
  expect_true(all(result[[1]] %in% c(parent1, parent2)))
  expect_true(all(result[[2]] %in% c(parent1, parent2)))
})
