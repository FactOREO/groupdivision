`%!in%` <- function(x, y) !(`%in%`(x, y))

#' Mutate Genes Function
#'
#' This function generates a random sample of genes from a given set of valid genes.
#'
#' @param len integer - An integer specifying the number of genes to sample
#' @param valid_genes vec<Optional<character|numeric>> - A vector of valid genes from which to sample
#'
#' @return A vector of randomly sampled genes.
mutate_genes <- function(len, valid_genes) {
  sample(x = valid_genes, size = len, replace = TRUE)
}

