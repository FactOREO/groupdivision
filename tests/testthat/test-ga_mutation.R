## === Flipping Mutation
test_that("flipping_mutation does not alter individual when mutation_probability is 0", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0
  result <- flipping_mutation(individual, mutation_probability)
  expect_equal(result, individual)
})

test_that("flipping_mutation flips all bits when mutation_probability is 1", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 1
  result <- flipping_mutation(individual, mutation_probability)
  expect_equal(result, !individual)
})

test_that("flipping_mutation returns the same result with the same seed", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0.5
  seed <- 123
  result1 <- flipping_mutation(individual, mutation_probability, seed)
  result2 <- flipping_mutation(individual, mutation_probability, seed)
  expect_identical(result1, result2)
})

test_that("flipping_mutation returns different results with different seeds", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0.5
  result1 <- flipping_mutation(individual, mutation_probability, seed = 123)
  result2 <- flipping_mutation(individual, mutation_probability, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("flipping_mutation correctly flips bits based on mutation_probability", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0.25
  seed <- 123
  set.seed(seed)
  probabilities <- runif(length(individual))
  expected <- sapply(seq_along(individual), function(i) ifelse(probabilities[[i]] > mutation_probability, individual[[i]], !individual[[i]]))
  result <- flipping_mutation(individual, mutation_probability, seed)
  expect_equal(result, expected)
})

test_that("flipping_mutation works for an individual with all TRUE values", {
  individual <- rep(TRUE, 10)
  mutation_probability <- 0.5
  result <- flipping_mutation(individual, mutation_probability)
  expect_length(result, length(individual))
  expect_true(all(result %in% c(TRUE, FALSE)))
})

test_that("flipping_mutation works for an individual with all FALSE values", {
  individual <- rep(FALSE, 10)
  mutation_probability <- 0.5
  result <- flipping_mutation(individual, mutation_probability)
  expect_length(result, length(individual))
  expect_true(all(result %in% c(TRUE, FALSE)))
})

test_that("flipping_mutation handles an empty individual", {
  individual <- logical(0)
  mutation_probability <- 0.5
  expect_error(flipping_mutation(individual, mutation_probability))
})

test_that("flipping_mutation handles mutation_probability of 0.5 correctly", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0.5
  result <- flipping_mutation(individual, mutation_probability)
  expect_length(result, length(individual))
  expect_true(all(result %in% c(TRUE, FALSE)))
})

test_that("flipping_mutation does not change length of individual", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  mutation_probability <- 0.5
  result <- flipping_mutation(individual, mutation_probability)
  expect_length(result, length(individual))
})


## === Inverse Mutation
test_that("inverse_mutation inverts the values between two indices", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  seed <- 123
  result <- inverse_mutation(individual, seed)
  expect_equal(result, c(TRUE, TRUE, FALSE, FALSE, TRUE))
})

test_that("inverse_mutation returns the same result with the same seed", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  seed <- 123
  result1 <- inverse_mutation(individual, seed)
  result2 <- inverse_mutation(individual, seed)
  expect_identical(result1, result2)
})

test_that("inverse_mutation returns different results with different seeds", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  result1 <- inverse_mutation(individual, seed = 123)
  result2 <- inverse_mutation(individual, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("inverse_mutation handles empty individual", {
  individual <- logical(0)
  expect_error(inverse_mutation(individual))
})

test_that("inverse_mutation handles single element individual", {
  individual <- c(TRUE)
  result <- inverse_mutation(individual)
  expect_equal(result, c(TRUE))
})

test_that("inverse_mutation handles two element individual", {
  individual <- c(TRUE, FALSE)
  result <- inverse_mutation(individual, seed = 123)
  expect_equal(result, c(FALSE, TRUE))
})

test_that("inverse_mutation correctly inverts entire vector", {
  individual <- rep(TRUE, 10)
  seed <- 1
  result <- inverse_mutation(individual, seed)
  expect_equal(result, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("inverse_mutation does not alter original individual length", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  result <- inverse_mutation(individual)
  expect_length(result, length(individual))
})

test_that("inverse_mutation correctly inverts when indices are at the edges", {
  individual <- c(TRUE, FALSE, TRUE, FALSE)
  seed <- 456
  result <- inverse_mutation(individual, seed)
  expect_equal(result, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("inverse_mutation correctly handles multiple inversions with same seed", {
  individual <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  seed <- 789
  result1 <- inverse_mutation(individual, seed)
  result2 <- inverse_mutation(result1, seed)
  expect_equal(result2, individual)
})

## === Random mutation
test_that("random_mutation does not alter individual when mutation_probability is 0", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0
  result <- random_mutation(individual, valid_genes, mutation_probability)
  expect_equal(result, individual)
})

test_that("random_mutation mutates all genes when mutation_probability is 1", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 1
  seed <- 123
  result <- random_mutation(individual, valid_genes, mutation_probability, seed)
  expect_false(any(result == individual))
  expect_true(all(result %in% valid_genes))
})

test_that("random_mutation returns the same result with the same seed", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.5
  seed <- 123
  result1 <- random_mutation(individual, valid_genes, mutation_probability, seed)
  result2 <- random_mutation(individual, valid_genes, mutation_probability, seed)
  expect_identical(result1, result2)
})

test_that("random_mutation returns different results with different seeds", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.5
  result1 <- random_mutation(individual, valid_genes, mutation_probability, seed = 123)
  result2 <- random_mutation(individual, valid_genes, mutation_probability, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("random_mutation handles empty individual", {
  individual <- numeric(0)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.5
  expect_error(random_mutation(individual, valid_genes, mutation_probability))
})

test_that("random_mutation handles single element individual", {
  individual <- c(1)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.5
  result <- random_mutation(individual, valid_genes, mutation_probability, seed = 123)
  expect_length(result, 1)
  expect_true(result %in% valid_genes)
})

test_that("random_mutation correctly mutates based on mutation_probability", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.25
  seed <- 123
  set.seed(seed)
  probabilities <- runif(length(individual))
  expected <- sapply(seq_along(individual), function(i) ifelse(probabilities[[i]] > mutation_probability, individual[[i]], sample(valid_genes[valid_genes != individual[[i]]], 1)))
  result <- random_mutation(individual, valid_genes, mutation_probability, seed)
  expect_equal(result, expected)
})

test_that("random_mutation does not change length of individual", {
  individual <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  valid_genes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  mutation_probability <- 0.5
  result <- random_mutation(individual, valid_genes, mutation_probability)
  expect_length(result, length(individual))
})

test_that("random_mutation handles case when valid_genes has only one gene", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(5)
  mutation_probability <- 1
  result <- random_mutation(individual, valid_genes, mutation_probability)
  expect_equal(result, rep(5, length(individual)))
})

test_that("random_mutation works correctly without specifying a seed", {
  individual <- c(1, 2, 3, 4)
  valid_genes <- c(1, 2, 3, 4, 5)
  mutation_probability <- 0.5
  result <- random_mutation(individual, valid_genes, mutation_probability)
  expect_length(result, length(individual))
  expect_true(all(result %in% valid_genes))
})

## === Scramble Mutation
test_that("scramble_mutation scrambles values between two indices", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 345
  result <- scramble_mutation(individual, seed)
  expect_equal(result, c(1, 2, 5, 3, 4))
})

test_that("scramble_mutation returns the same result with the same seed", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 123
  result1 <- scramble_mutation(individual, seed)
  result2 <- scramble_mutation(individual, seed)
  expect_identical(result1, result2)
})

test_that("scramble_mutation returns different results with different seeds", {
  individual <- c(1, 2, 3, 4, 5)
  result1 <- scramble_mutation(individual, seed = 123)
  result2 <- scramble_mutation(individual, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("scramble_mutation handles empty individual", {
  individual <- numeric(0)
  expect_error(scramble_mutation(individual))
})

test_that("scramble_mutation handles single element individual", {
  individual <- c(1)
  result <- scramble_mutation(individual)
  expect_equal(result, c(1))
})

test_that("scramble_mutation handles two element individual", {
  individual <- c(1, 2)
  result <- scramble_mutation(individual, seed = 123)
  expect_true(all(result %in% individual))
  expect_length(result, 2)
})

test_that("scramble_mutation correctly scrambles entire vector", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 1
  result <- scramble_mutation(individual, seed)
  expect_true(all(result %in% individual))
  expect_length(result, length(individual))
})

test_that("scramble_mutation does not alter original individual length", {
  individual <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- scramble_mutation(individual)
  expect_length(result, length(individual))
})

test_that("scramble_mutation correctly scrambles when indices are at the edges", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 456
  result <- scramble_mutation(individual, seed)
  expect_true(all(result %in% individual))
  expect_length(result, length(individual))
})

test_that("scramble_mutation correctly handles multiple scrambles with same seed", {
  individual <- c(1, 2, 3, 4, 5, 6, 7, 8)
  seed <- 789
  result1 <- scramble_mutation(individual, seed)
  result2 <- scramble_mutation(result1, seed)
  expect_equal(result2, individual)
})

## === Swap Mutation
test_that("swap_mutation swaps two elements in the individual", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 123
  result <- swap_mutation(individual, seed)
  expect_equal(result, c(1, 3, 2, 4, 5))
})

test_that("swap_mutation returns the same result with the same seed", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 123
  result1 <- swap_mutation(individual, seed)
  result2 <- swap_mutation(individual, seed)
  expect_identical(result1, result2)
})

test_that("swap_mutation returns different results with different seeds", {
  individual <- c(1, 2, 3, 4, 5)
  result1 <- swap_mutation(individual, seed = 123)
  result2 <- swap_mutation(individual, seed = 456)
  expect_false(identical(result1, result2))
})

test_that("swap_mutation handles empty individual", {
  individual <- numeric(0)
  expect_error(swap_mutation(individual))
})

test_that("swap_mutation handles single element individual", {
  individual <- c(1)
  result <- swap_mutation(individual)
  expect_equal(result, c(1))
})

test_that("swap_mutation handles two element individual", {
  individual <- c(1, 2)
  result <- swap_mutation(individual, seed = 123)
  expect_equal(result, c(2, 1))
})

test_that("swap_mutation does not alter original individual length", {
  individual <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  result <- swap_mutation(individual)
  expect_length(result, length(individual))
})

test_that("swap_mutation correctly swaps when indices are at the edges", {
  individual <- c(1, 2, 3, 4, 5)
  seed <- 456
  result <- swap_mutation(individual, seed)
  expect_true(all(result %in% individual))
  expect_length(result, length(individual))
})

test_that("swap_mutation correctly handles multiple swaps with same seed", {
  individual <- c(1, 2, 3, 4, 5, 6, 7, 8)
  seed <- 789
  result1 <- swap_mutation(individual, seed)
  result2 <- swap_mutation(result1, seed)
  expect_false(identical(result1, result2))
})
