#' Parse a configuration part from items or translogs
#'
#' Function to parse a given config received via config::get() with the
#' necessary structure provided by the example configuration. Returns a named
#' list of all valid arguments needed for the genetic algorithm funcitonality
#'
#' @param config A configuration object
#'
#' @return list - Configuration list of a data block
#'
#' @export
parse_data_config <- function(config) {
  if (!config[["enabled"]] %||% FALSE) {
    logger::log_info("Will not parse config because enabled is set to FALSE")
    return(invisible(NULL))
  }
  out <- list()
  ## Parse numerical variables and the weight column to use if any
  numerical_vars <- get_numerical_vars(config)
  out <- append(out, numerical_vars)
  ## Parse categorical variables
  categorical_vars <- get_categorical_vars(config)
  out <- append(out, categorical_vars)
  ## Define the aggregator inputs
  out$master_col <- unlist(lapply(config[["columns"]], function(x) {
    if (isTRUE(x[["is_master"]])) x[["column"]]
  }))
  out$variant_col <- unlist(lapply(config[["columns"]], function(x) {
    if (isTRUE(x[["is_variant"]])) x[["column"]]
  }))
  out$time_col <- unlist(lapply(config[["columns"]], function(x) {
    if (x[["type"]] %in% c("IDate", "Date")) x[["column"]]
  }))
  out$label_col <- unlist(lapply(config[["columns"]], function(x) {
    if (!is.null(x[["label"]])) x[["column"]]
  }))
  out
}

#' Parse all numerical variables from a given configuration
#'
#' Function to extract, parse and return all numerical variables within a
#' provided configuration. See example configuration.
#' Returns a list of vectors for numerical columns, weights, methods as well
#' as the weight column.
#'
#' @param config A configuration list
#'
#' @return list<vec<character|bool>>
get_numerical_vars <- function(config) {
  numerical_vars <- get_vars_by_type(config, types = c("numeric", "integer"))
  numerical_methods <- get_aggregation_methods(config, numerical_vars)
  numerical_weights <- get_use_of_weights(config, numerical_vars)
  weight_vars <- get_aggregation_weights(config, numerical_vars)

  list(numerical_vars = numerical_vars,
       numerical_methods = numerical_methods,
       numerical_weights = numerical_weights,
       weight_vars = weight_vars)
}

#' Parse all categorical attributes from a given configuration
#'
#' Function to extract, parse and return all categorical variables within a
#' provided configuration. See exmaple configuration.
#'
#' @param config A configuration list
get_categorical_vars <- function(config) {
  categorical_vars <- get_vars_by_type(config, "character")
  categorical_methods <- get_aggregation_methods(config, categorical_vars)
  categorical_weights <- get_use_of_weights(config, categorical_vars)

  list(categorical_vars = categorical_vars,
       categorical_methods = categorical_methods,
       categorical_weights = categorical_weights)
}

#' Helper function to receive columns from config by type
#'
#' @param config: list A configuration list
#' @param types: vec<character> A vector of types to get variables from
#'
#' @return Optional<vec<character>, NULL>
#'
#' @import logger
get_vars_by_type <- function(config, types = NULL) {
  types %||% return(NULL)
  vars <- unlist(lapply(config[["columns"]], function(x) {
    column <- x[["column"]]
    col_type <- x[["type"]]
    is_master <- x[["is_master"]] %||% FALSE
    is_variant <- x[["is_variant"]] %||% FALSE
    if (col_type %in% types && !is_master && !is_variant) column
  }))
  logger::log_debug("Found {length(vars)} columns matching {paste0(types, collapse = '|')}")
  if (length(vars) > 0) vars else NULL
}

#' Helper function to parse methods from config
#'
#' @param config A configuration list
#' @param cols A vector of column names
#'
#' @return Optional<vec<character>, NULL>
get_aggregation_methods <- function(config, cols) {
  cols %||% return(NULL)
  sapply(
    X = config[["columns"]],
    FUN = \(x) {
      column = x[["column"]]
      method = x[["method"]]
      if (column %in% cols) method
    },
    USE.NAMES = FALSE
  ) |> unlist()
}

#' Helper function to parse weights from config
#'
#' This functions extracts the information about the use of a weight
#' from the given configuration for all numerical columns
#'
#' @param config A configuration list
#' @param cols A vector of column names
#'
#' @return Optional<vec<bool>,NULL>
get_use_of_weights <- function(config, cols) {
  cols %||% return(NULL)
  ## Return a boolean vector of length num_cols
  sapply(
    X = config[["columns"]],
    FUN = \(x) {
      if (x[["column"]] %in% cols) {
        use_weight <- x[["use_weight"]] %||% FALSE
        if (use_weight) return(TRUE)
        FALSE
      }
    },
    USE.NAMES = FALSE
  ) |> unlist()
}

#' Helper function to find the columns specified as weights
#'
#' @param config A configuration list
#' @param cols A vector of column names
#'
#' @return Optional<vec<character>,NULL>
get_aggregation_weights <- function(config, cols) {
  cols %||% return(NULL)
  sapply(
    X = config[["columns"]],
    FUN = \(x) {
      column <- x[["column"]]
      is_weight <- x[["is_weight"]] %||% FALSE
      if (column %in% cols && is_weight) column
    },
    USE.NAMES = FALSE
  ) |> unlist()
}


#' Helper function to receive the algorithm config
#'
#' @param config list - A configuration list, see example configuration
#'
#' @return list
#'
#' @import logger
get_strategy_config <- function(config) {
  strategy <- config[["strategy"]]
  config[[strategy]] %||% {
    logger::log_error("Specified strategy `{strategy}` is not present as an option with arguments!")
    stop("Specified strategy is not present as an option with arguments!")
  }
  config[[strategy]]
}
