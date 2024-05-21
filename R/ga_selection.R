# This module contains all available functions to run the selection of a given set of individuals with their
# fitness scores. Currently implemented are
# * random selection
# * steady state selection
# * tournament selection
# All provided functions return a vector containing the indices of the individuals to keep from the provided
# vector of fitness values.

#' Random selection
#'
#' This function selects a random set of individuals from a given
#' vector of fitness scores. The function optionally accepts a seed for deterministic outcomes.
#'
#' @param fitness_values vec<numeric> - A vector of numeric values representing the individuals fitness values
#' @param num_parents integer - The number of individuals to choose
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return vec<integer>
random_selection <- function(fitness_values, num_parents, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(fitness_values) == 0L) stop("No fitness values provided")
  if (num_parents == 0L) stop("Number of parents to select cannot be 0!")
  if (length(fitness_values) == 1L) return(1L)
  sample(seq_along(fitness_values), num_parents)
}

#' Steady state selection
#'
#' This selection method will always return the top n individuals from a given vector
#' of fitness values provided.
#'
#' @param fitness_values Vec<numeric> - A vector of numeric values representing the individuals fitness values
#' @param num_parents integer - The number of individuals to choose
#'
#' @return vec<integer>
steady_state_selection <- function(fitness_values, num_parents, seed = NULL) {
  if (length(fitness_values) == 0L) stop("No fitness values provided")
  if (num_parents == 0L) stop("Number of parents to select cannot be 0!")
  if (length(fitness_values) == 1L) return(1L)
  fitness_matrix <- matrix(c(fitness_values, seq_along(fitness_values)), ncol = 2)
  fitness_matrix <- fitness_matrix[order(fitness_matrix[, 1], decreasing = TRUE), ]
  if (num_parents >= length(fitness_values)) return(fitness_matrix[seq.int(1L, length(fitness_values), 1L), 2L])
  fitness_matrix[seq.int(1L, num_parents, 1L), 2L]
}

#' Tournament selection
#'
#' Perform \code{num_parents} tournamens of size \code{tournament_size}. For each tournament, a random selection
#' of size \code{torunament_size} individuals are chosen. The fittest individual within the tournament is added to the vector of chosen individuals.
#'
#' @param fitness_values Vec<numeric> - A vector of numeric values representing the individuals fitness values
#' @param num_parents integer - The number of individuals to choose
#' @param tournament_size integer - The size of each tournament
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return vec<integer>
tournament_selection <- function(fitness_values, num_parents, tournament_size, seed = NULL) {
  if (length(fitness_values) == 0L) stop("No fitness values provided")
  if (num_parents == 0L) stop("Number of parents to select cannot be 0!")
  if (length(fitness_values) == 1L) return(1L)
  fitness_matrix <- matrix(c(fitness_values, seq_along(fitness_values)), ncol = 2)
  if (tournament_size > length(fitness_values)) return(steady_state_selection(fitness_values, 1L, seed))
  if (tournament_size == 1L) return(random_selection(fitness_values, num_parents, seed))
  if (!is.null(seed)) set.seed(seed)
  winners <- replicate(n = num_parents, expr = {
    competitors <- fitness_matrix[sample(nrow(fitness_matrix), tournament_size), ]
    competitors[order(competitors[, 1L], decreasing = TRUE), ][[1L, 2L]]
  })
  unique(winners)
}
