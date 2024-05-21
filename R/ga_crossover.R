# This module contains all available options for the crossover step within the genetic algorithm.
# Currently implemented are
# * single-point crossover
# * multi-point crossover
# * shuffle crossover
# * uniform crossover
# All functions return a list containing two vectors of type <Some> depending on your input.

#' Single Point Crossover
#'
#' This functions performs a single point crossover from to given parents and returns two new children.
#' The crossover point is chosen randomly, but can be mande deterministic by providing a seed value.
#'
#' @param parent1 vec<Some> - Vector containing the genome information of parent 1
#' @param parent2 vec<Some> - Vector containing the genome information of parent 2
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<error|list<vec<bool>,vec<bool>>>
single_point_crossover <- function(parent1, parent2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(parent1) == 0L) stop("Cannot handle empty parents")
  if (length(parent1) != length(parent2)) stop("Parents must have the same length!")
  len <- length(parent1)
  if (len == 1L) return(list(parent1, parent2))
  idx <- sample(len - 1L, 1L)
  children1 <- c(parent1[seq.int(1L, idx)], parent2[seq.int(idx + 1L, len)])
  children2 <- c(parent2[seq.int(1L, idx)], parent1[seq.int(idx + 1L, len)])
  list(children1, children2)
}

#' Multiple Points Crossover
#'
#' This functions performs a multiple points crossover from to given parents and returns two new children.
#' The \code{k} crossover points are chosen randomly, but can be mande deterministic by providing a seed value.
#' Only usable in the context of a bivariat target space of valid genes for combination.
#'
#' @param parent1 vec<Some> - Vector containing the genome information of parent 1
#' @param parent2 vec<Some> - Vector containing the genome information of parent 2
#' @param k integer - The number of crossover points
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<error|list<vec<bool>,vec<bool>>>
multi_point_crossover <- function(parent1, parent2, k, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(parent1) == 0L) stop("Cannot handle empty parents")
  if (length(parent1) != length(parent2)) stop("Parents must have the same length!")
  len <- length(parent1)
  if (k >= len) return(list(parent1, parent2))
  idxs <- sort(sample(len - 1L, k))
  children1 <- parent1
  children2 <- parent2
  for (i in seq_along(idxs)) {
    if (i == length(idxs)) {
      curr_idx <- idxs[[i]]
      next_idx <- len
    } else {
      curr_idx <- idxs[[i]]
      next_idx <- idxs[[i + 1L]] - 1L
    }
    if (i %% 2L != 0L) {
      children1[curr_idx:next_idx] <- parent1[curr_idx:next_idx]
      children2[curr_idx:next_idx] <- parent2[curr_idx:next_idx]
    } else {
      children1[curr_idx:next_idx] <- parent2[curr_idx:next_idx]
      children2[curr_idx:next_idx] <- parent1[curr_idx:next_idx]
    }
  }
  list(children1, children2)
}

#' Shuffled Crossover
#'
#' Perform a shuffled crossover, e.g. shuffle all genes from both parents repectively and perform a single point
#' crossover to return two new children. Accepts an optional seed for deterministic outcomes.
#'
#' @param parent1 vec<Some> - Vector containing the genome information of parent 1
#' @param parent2 vec<Some> - Vector containing the genome information of parent 2
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<error|list<vec<bool>,vec<bool>>>
shuffled_crossover <- function(parent1, parent2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(parent1) == 0L) stop("Cannot handle empty parents")
  if (length(parent1) != length(parent2)) stop("Parents must have the same length!")
  len <- length(parent1)
  shuffled_parent1 <- parent1[sample(len, len)]
  shuffled_parent2 <- parent1[sample(len, len)]
  single_point_crossover(shuffled_parent1, shuffled_parent2, seed)
}

#' Uniform Crossover
#'
#' Performs a crossover between two individuals by determining on each gene a random point, comparing to a given
#' probability between 0 and 1. If the value is above the given probability, the a crossover happens,
#' else the gene remains stable. Accepts a seed to provide deterministic outcomes if needed.
#'
#' @param parent1 vec<Some> - Vector containing the genome information of parent 1
#' @param parent2 vec<Some> - Vector containing the genome information of parent 2
#' @param proability numeric - Numerical value between 0 and 1
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<error|list<vec<bool>,vec<bool>>>
uniform_crossover <- function(parent1, parent2, probability = 0.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (probability > 1 || probability < 0) stop("probability must be between 0 and 1!")
  if (length(parent1) == 0L) stop("Cannot handle empty parents")
  if (length(parent1) != length(parent2)) stop("Parents must have the same length!")
  len <- length(parent1)
  probabilities <- runif(len)
  children1 <- sapply(seq_along(parent1), \(i) if (probabilities[[i]] > probability) parent2[[i]] else parent1[[i]])
  children2 <- sapply(seq_along(parent1), \(i) if (probabilities[[i]] > probability) parent1[[i]] else parent2[[i]])
  list(children1, children2)
}
