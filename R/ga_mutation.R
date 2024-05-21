# Tis module contains all functions to perform mutations of a given genome. There are currently the following
# mutation algorithms implemented:
# * flipping mutation
# * inversion mutation
# * random mutation
# * scramble mutation
# * swap mutation
# All provided funtions return a vector of type <Some> depending on your input.

#' Flipping Mutation
#'
#' Flipping mutation is a mutation only for binary encoded individuals. Given the mutation_probability and individual
#' it iterates through the boolean vector of individual and generates a random number between 0.0 and 1.0. If the
#' number is less than mutation_probability then we flip the value at that index else it remains the same.
#' The function can also take in an optional seed value for deterministic results.
#'
#' @param individual vec<bool> - A boolean vector representating the genome information
#' @param mutation_probability numeric - The probability of a mutation happening
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<err|vec<bool>>
flipping_mutation <- function(individual, mutation_probability = 0.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  len <- length(individual)
  if (len == 0L) stop("Individual has no genome provided")
  probabilities <- runif(len)
  sapply(seq_along(individual), \(i) ifelse(probabilities[[i]] > mutation_probability, individual[[i]], !individual[[i]]))
}

#' Inverse Mutation
#'
#' Inversion mutation is a mutation only for binary encoded individuals. Given the individual it randomly generates
#' two indices and then inverts the value between those indices of the individual. The function can also take in an
#' optional seed value for deterministic results.
#'
#' @param individual vec<bool> - A boolean vector representating the genome information
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<err|vec<bool>>
inverse_mutation <- function(individual, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  len <- length(individual)
  if (len == 0L) stop("Individual has no genome provided")
  if (len == 1L) return(individual)
  if (len == 2L) return(!individual)
  idxs <- sort(sample(len, 2L))
  individual[idxs[[1L]]:idxs[[2L]]] <- !individual[idxs[[1L]]:idxs[[2L]]]
  individual
}

#' Random Mutation
#'
#' Random mutation is for a valid gene set larger than two. Given the set of valid genes as well as a mutation
#' probability this function mutates a specific genome if the drawn probability exceeds the provided mutation
#' probability. with any from the given valid genes except the current one present. The function can also take in an
#' optional seed value for deterministic results.
#'
#' @param individual vec<bool> - A boolean vector representating the genome information
#' @param valid_genes vec<Some> - Vector of possible values for a gene
#' @param mutation_probability numeric - The probability of a mutation happening
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<err|vec<bool>>
random_mutation <- function(individual, valid_genes = c(TRUE, FALSE), mutation_probability = 0.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  len <- length(individual)
  if (len == 0L) stop("Individual has no genome provided")
  if (length(valid_genes) == 1L) return(rep(valid_genes, len))
  probabilities <- runif(len)
  sapply(seq_along(individual),
         \(i) {
           ifelse(probabilities[[i]] > mutation_probability,
             individual[[i]],
             sample(valid_genes[valid_genes != individual[[i]]], 1L)
           )
         })
}

#' Scramble Mutation
#'
#' This mutation algorithm generates two indices randomly, than shuffles all genes inbetween those indices
#' randomly. The function can also take in an optional seed value for deterministic results.
#'
#' @param individual vec<bool> - A boolean vector representating the genome information
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<err|vec<bool>>
scramble_mutation <- function(individual, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  len <- length(individual)
  if (len == 0L) stop("Individual has no genome provided")
  if (len == 1L) return(individual)
  idxs <- sort(sample(len, 2L))
  individual[idxs[[1L]]:idxs[[2L]]] <- individual[sample(seq.int(idxs[[1L]], idxs[[2L]]), idxs[[2L]] - idxs[[1L]] + 1L)]
  individual
}

#' Swap Mutation
#'
#' This mutation algorithm generates two indices randomly, than swaps all values between the two generated indices.
#' The function can also take in an optional seed value for deterministic results.
#'
#' @param individual vec<bool> - A boolean vector representating the genome information
#' @param seed Optional<NULL|integer> - A random seed for deterministic outcomes
#'
#' @return Optional<err|vec<bool>>
swap_mutation <- function(individual, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  len <- length(individual)
  if (len == 0L) stop("Individual has no genome provided")
  if (len == 1L) return(individual)
  idxs <- sample(len, 2L)
  val1 <- individual[[idxs[[1L]]]]
  val2 <- individual[[idxs[[2L]]]]
  individual[[idxs[[1L]]]] <- val2
  individual[[idxs[[2L]]]] <- val1
  individual
}
