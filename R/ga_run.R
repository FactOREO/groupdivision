#' Execute the genetic algorithm
#'
#' This function uses a given data set and it's corresponding values from a given configuration list
#' to optimally reallocate the given groups w.r.t. to all variables to be considered.
#' This function is capable of utilizing numerical as well as categorical variables by aggregating them
#' to obtain comparable matrices.
#'
#' @param df data.table::data.table - Object containing the data to be splitted into groups
#' @param config list - A list containing all relevant parameters for the genetic algorithm to run (see example  config)
#' @param numerical_vars Optional<vec<character>|NULL> - A vector of column names to be considered numeric
#' @param categorical_vars Optional<vec<character>|NULL> - A vector of column names to be considered categorical
#'
#' @return Optional<numeric|error>
run_genetic_algorithm <- function(input_data, config, valid_genes = NULL, numerical_vars = NULL, categorical_vars = NULL) {
  if (is.null(df) || nrow(df) == 0L) {
    logger::log_fatal("Empty data passed to `run_genetic_algorithm`")
    stop("Empty data passed to `run_genetic_algorithm`")
  }
  if (all_null(numerical_vars, categorical_vars)) {
    logger::log_fatal("No columns passed for division calculations")
    stop("No columns passed for division calculations")
  }
  ## Define the length of each genome based on the number of data points passed
  genome_length <- nrow(input_data)
  ## Get all initial values from the "initial_values" section
  population_size <- config[["initial_values"]][["population_size"]] %||% {
    ## It is reported in Alander’s study [1], that a value between n and 2n is optimal for the population size
    ## (https://ink.library.smu.edu.sg/cgi/viewcontent.cgi?article=1840&context=lkcsb_research)
    logger::log_warn("No initial population size was passed - default to twice the genome length ({genome_length})")
    2 * genome_length
  }
  if (population_size > 2 * genome_length) {
    logger::log_warn("The population size exceeds twice the genome_length of {genome_length}. This could be inefficient!")
  }
  ## Valid genes depend on the number of groups
  group_size <- config[["initial_values"]][["group_size"]] %||% {
    logger::log_warn("No group size chosen - default to 2!")
    2L
  }
  logger::log_info("Calculate {group_size} distinct groups")
  if (group_size > 2) {
    valid_genes <- seq.int(1L, group_size, 1L)
  } else {
    valid_genes <- c(TRUE, FALSE)
  }
  logger::log_info("Valid genes are {paste(as.character(valid_genes), collapse = ',')}")
  ## Set the initial population
  # Each column is one individual with genome_length values
  population <- replicate(population_size,  sample(valid_genes, genome_length, TRUE))

  ## Mutation step
  mutation_method <- config[["mutation"]][["method"]]
  if (group_size > 2 && mutation_method %in% c("flipping", "inversion")) {
    logger::log_fatal("Invalid mutation method `{mutation_method}` for non-binary groups chosen!")
    stop(sprintf("Invalid mutation method `%s` for non-binary groups chosen!", mutation_method))
  }
  mutation_probability <- config[["mutation"]][["mutation_probability"]]
  mutation_seed <- config[["mutation"]][["seed"]]

  ## Selection step
  selection_method <- config[["selection"]][["method"]]
  num_parents <- config[["selection"]][["num_parents"]]
  if (selection_method == "tournament") {
    tournament_size <- config[["selection"]][["tournament_size"]] %||% {
      logger::log_warn("No tournament size specified - default to 100")
      100L
    }
  }
  selection_seed <- config[["selection"]][["seed"]]

  ## Crossover step
  crossover_method <- config[["crossover"]][["method"]]
  crossover_probability <- config[["crossover"]][["crossover_probability"]] %||% {
    logger::log_warn("No crossover probability specified - default to 0.5")
    0.5
  }
  crossover_seed <- config[["crossover"]][["seed"]]
  if (crossover_method == "multi_point") {
    k <- config[["crossover"]][["k"]] %||% {
      logger::log_warn("No k for multi point crossover specified - default to 2")
      2L
    }
  }

  ## Stopping conditions from config
  stop_after_n_local_optim <- config[["exit_condition"]][["n_local_optim"]] %||% {
    logger::log_info("No exit condition w.r.t. local optima given - default to 5")
    5L
  }
  stop_after_n_runs <- config[["exit_condition"]][["n_cycles"]] %||% {
    logger::log_info("No exit condition w.r.t. number of cycles given - default to 20")
    20L
  }

  ## The stopping condition relies on the following variables
  winner <- NULL
  current_run <- 0L
  curr_local_optim_count <- 0L
  fittest_value <- Inf
  max_local <- 1L
  curr_min <- Inf

  ## Run the algorithm
  exit_condition <- (current_run >= stop_after_n_runs) || (curr_local_optim_count >= stop_after_n_local_optim)
  while (!exit_condition) {
    current_run <- current_run + 1L
    logger::log_info("Begin run no. {current_run} with {dim(population)[[2L]]} inividuals")
    logger::log_info("Begin fitness score evaluation")
    fitness_scores <- future.apply::future_apply(
      population, 2, \(idv) calc_fitness_score(idv, df, categorical_vars, numerical_vars), future.seed = TRUE
    )
    logger::log_info("Finished fitness score evaluation")
    last_winner <- winner
    last_fittest_value <- fittest_value
    winner <- population[, which.max(fitness_scores)]
    fittest_value <- max(fitness_scores)
    logger::log_info("Fittest individual has score {round(fittest_value, 2)}")
    if (last_fittest_value == fittest_value) {
      curr_local_optim_count <- curr_local_optim_count + 1L
      logger::log_info("Detected local optima no. {curr_local_optim_count}")
      if (identical(last_winner, winner)) {
        logger::log_info("Last and current winner are identical")
      }
    } else {
      curr_local_optim_count <- 0L
    }
    ## Update exit condition
    exit_condition <- (current_run >= stop_after_n_runs) || (curr_local_optim_count >= stop_after_n_local_optim)
    if (exit_condition) {
      logger::log_info("Fittest individual after {current_run} iterations: {round(fittest_value, 2)}")
      return(winner)
    }
    logger::log_info("Begin selection process")
    selected_parents <- switch(
      selection_method,
      "random" = random_selection(fitness_scores, num_parents, selection_seed),
      "steady_state" = steady_state_selection(fitness_scores, num_parents, selection_seed),
      "tournament" = tournament_selection(fitness_scores, num_parents, tournament_size, selection_seed),
      {
        logger::log_fatal("Unrecognized selection method `{selection_method}`")
        stop(sprintf("Unrecognized selection method `%s`", selection_method))
      }
    )
    logger::log_info("Finished selection process")
    logger::log_info("Begin the crossover of chosen individuals")
    children <- do.call(cbind, future.apply::future_lapply(
      seq.int(floor((population_size - num_parents) / 2L)),
      function(i) {
        parent1 <- population[, sample(selected_parents, 1L)]
        parent2 <- population[, sample(selected_parents, 1L)]
        children <- switch(
          crossover_method,
          "single_point" = single_point_crossover(parent1, parent2, crossover_seed),
          "multi_point" = multi_point_crossover(parent1, parent2, k, crossover_seed),
          "shuffle" = shuffled_crossover(parent1, parent2, crossover_seed),
          "uniform" = uniform_crossover(parent1, parent2, crossover_probability, crossover_seed),
          {
            logger::log_fatal("Unrecognized crossover method `{crossover_method}`")
            stop(sprintf("Unrecognized crossover method `%s`", crossover_method))
          }
        )
        do.call(cbind, children)
      },
      future.seed = TRUE
    ))
    logger::log_info("Finished the crossover of chosen individuals")
    logger::log_info("Begin mutation of generated children")
    children <- future.apply::future_apply(
      children, 2L,
      function(idv) {
        switch(
          mutation_method,
          "flipping" = flipping_mutation(idv, mutation_probability, mutation_seed),
          "inversion" = inverse_mutation(idv, mutation_seed),
          "random" = random_mutation(idv, valid_genes, mutation_probability, mutation_seed),
          "scramble" = scramble_mutation(idv, mutation_seed),
          "swap" = swap_mutation(idv, mutation_seed),
          {
            logger::log_warn("Unrecognized mutation method `{mutation_method}`")
            stop(sprintf("Unrecognized mutation method `%s`", mutation_method))
          }
        )
      },
      future.seed = TRUE
    )
    logger::log_info("Finished mutation of generated children")
    # Combine the parents and the children
    population <- cbind(population[, selected_parents], children)
    logger::log_info("Finished run no. {current_run}")
  }
}
