### ==================================================================================================================
# Author: Dustin Hennig
# Role: Data Scientist at GK Artificial Intelligence for Retail AG
# Date: May 8th 2024
#
# This file contains the logics necessary for the aggregation steps of all provided data sets within the
# applications usage. The core component is the aggregator function, to efficiently aggregate a provided
# input data.table::data.table object utilizing the collapse::collap functionality.
### ==================================================================================================================

#' Agggregate target input data
#'
#' This function takes at least one index column as aggregation level as well as at least on numerical or categorical
#' variable as input arguments with their respective aggregation functions. The aggregation method for categorical
#' values defaults to their mode, e.g. the most frequent value. The aggregator is capable of advanced weighted and
#' grouped custom aggregations by construction a call to the \code{custom} argument from \code{collapse::collap()}.
#' Possible arguments are existing columns within the given data.frame for \code{index_vars},
#' \code{numerical_columns}, \code{categorical_columns} and \code{weight_vars}. Multiple weight columns can be
#' chosen. Possible aggregation methods for numerical columns are \code{fsum}, \code{fmean}, \code{fmax},
#' \code{fmin}, \code{fmedian}, \code{fsd}, \code{fvar} or \code{flast}. All mentioned columns originate in the
#' \code{collapse} R package for fast statistical programming. With the optional parameter \code{use_weight} it can be
#' specified if weights should apply and if yes, to which columns. Default is \code{NULL} for no weighted aggregations.
#' The parameter can handle a vector of length \code{length(c(numerical_columns, categorical_columns))} if you wish to
#' perform weighted aggregations with the chosen method only on a subset of the passed data.
#'
#' @param df data.table::data.table - An object of type data.frame
#' @param index_vars vec<character> - A column name or vector of column names present in \code{data} to aggregate by
#' @param numerical_vars vec<character> - A vector of column names for numerical aggregations
#' @param categorical_vars vec<character> - A vector of column names for categorical aggregations
#' @param agg_methods_num vec<character> - A vector of aggregation methods for each specified column in \code{numerical_vars}
#' @param agg_methods_categorical vec<character> - A vector of aggregation methods for each specified column in \code{categorical_vars}
#' @param weight_vars vec<character> - One or more present columns in the passed \code{df} which should be used for weighting
#' @param use_weight A logical vector with length \code{length(c(numerical_vars, categorical_vars))} or a single
#' \code{TRUE} / \code{FALSE} for global option setting if weights should be used
#' @param ... Additional parameters which can be passed to \code{collapse::collap()}
#'
#' @import data.table
#' @export
aggregator <- function(
  df,                      # data.table::data.table
  index_vars       = NULL, # vec<character>
  numerical_vars   = NULL, # vec<character>
  categorical_vars = NULL, # vec<character>
  agg_methods_num  = NULL, # vec<character>
  agg_methods_cat  = NULL, # vec<character>
  weight_vars      = NULL, # vec<character>
  use_weight       = FALSE, # vec<bool>
  ...                      # Further arguments passed to collapse::collap()
) {                        # -> data.table::data.table
  ## Validate the input arguments w.r.t. index, numerical and categorical variables
  validate_aggregator_input(index_vars, numerical_vars, agg_methods_num, categorical_vars, agg_methods_cat)

  ## Check if any columns for aggregation are given
  if (all_null(numerical_vars, categorical_vars)) {
    logger::log_debug("No columns to aggregate specified - return input data")
    return(df)
  }

  ## Catch the case that use_weight was specified but within the configuration there was no weight column
  ## given. Emit a warning and continue with `use_weight` FALSE.
  weight_vars %||% {
    if (!all_false(use_weight)) {
      logger::log_warn("`use_weight` specified but no weight column(s) given - default to all FALSE!")
      use_weight <- FALSE
    }
  }

  ## Utilize the helper function `create_formula` to specifiy the aggregation formula to use by
  ## collapse::collap() for the aggregation.
  by_formula <- create_formula(index_vars, c(numerical_vars, categorical_vars))

  ## If there are any weight columns given, we want to create a formula for the weight
  ## variables as well.
  if (! is.null(weight_vars)) weight_formula <- create_formula(weight_vars)
  if (length(weight_vars) > 1) logger::log_warn("More than one weight specified!")

  ## Log all specified settings for debugging
  logger::log_debug("Raw data:\n{as.character(str(data))}")
  logger::log_debug("By Formula: {deparse(by_formula)}")
  logger::log_debug("Numerical columns: {paste0(numerical_vars, ' (', agg_methods_num, ')', collapse = '|')}")
  logger::log_debug("Categorical columns: {paste0(categorical_vars, ' (', agg_methods_cat, ')', collapse = '|')}")
  logger::log_debug("Weight Column(s): {paste(weight_vars, collapse = '|')}")
  logger::log_debug("Use-Weights: {paste(use_weight, collapse = '|')}")

  ## Create a custom list containing all the specified settings for the aggregations to pass to collapse::collap()
  custom_list <- data.frame(
    col    = c(numerical_vars, categorical_vars),
    method = c(agg_methods_num, agg_methods_cat)
  ) |>
    collapse::fmutate(
      use_weight = if (!is.null(use_weight)) use_weight else FALSE,
      num = c(rep(TRUE, length(numerical_vars)), rep(FALSE, length(categorical_vars))),
      func = data.table::fcase(
        use_weight == TRUE & num == TRUE, method,
        num == TRUE, paste(method, "uw", sep = "_"),
        num == FALSE, method
      )
    ) |>
    collapse::rsplit(~ func) |>
    collapse::rapply2d(function(x) paste(x[["col"]]))
  logger::log_debug("Custom list:\n\n{as.character(str(custom_list))}")

  ## Call the aggregation function collapse::collap()
  collapse::collap(X        = df,
                   by       = by_formula,
                   w        = if (!is.null(weight_vars)) weight_formula else NULL,
                   custom   = custom_list,
                   keep.w   = if (!is.null(weight_vars)) FALSE else NULL,
                   ...)
}

#' Create a formula object
#'
#' Helper function to create a formula object based on one or two character vectors
#'
#' @param right_side vec<character>
#' @param left_side vec<character>
#'
#' @return formula
create_formula <- function(right_side, left_side = NULL) {
  logger::log_debug("Left side: {paste(left_side, collapse = ',')}| Right side: {paste(right_side, collapse = ',')}")
  as.formula(
    paste(paste(gsub("^(.*?)$", "`\\1`", left_side), collapse = " + "),
          " ~ ",
          paste(gsub("^(.*?)$", "`\\1`", right_side), collapse = " + "))
  )
}


#' Validate input arguments for the aggregator
#'
#' This function performs a small set of sanitizer tasks to check if the provided input is
#' viable for the aggregator to run.
#'
#' @param index_vars vec<character> - A column name or vector of column names present in \code{data} to aggregate by
#' @param numerical_vars vec<character> - A vector of column names for numerical aggregations
#' @param categorical_vars vec<character> - A vector of column names for categorical aggregations
#' @param agg_methods_num vec<character> - A vector of aggregation methods for each specified column in \code{numerical_vars}
#' @param agg_methods_categorical vec<character> - A vector of aggregation methods for each specified column in \code{categorical_vars}
validate_aggregator_input <- function(
  index_vars       = NULL, # vec<character>
  numerical_vars   = NULL, # vec<character>
  agg_methods_num  = NULL, # vec<character>
  categorical_vars = NULL, # vec<character>
  agg_methods_cat  = NULL  # vec<character>
) {                        # Optional<None|Error>
  index_vars %||% {
    logger::log_error("No aggregation columns given!")
    stop("No aggregation columns given!")
  }
  if (length(numerical_vars) != length(agg_methods_num)) {
    logger::log_fatal("Length mismatch between numerical_vars and agg_methods_num!\n{paste0(numerical_vars, , ' (', agg_methods_num, ')', collapse = ',')}")
    stop("Length mismatch between numerical_vars and agg_methods_num!")
  }
  if (length(categorical_vars) != length(agg_methods_cat)) {
    logger::log_fatal("Length mismatch between categorical_vars and agg_methods_cat!\n{paste0(categorical_vars, , ' (', agg_methods_cat, ')', collapse = ',')}")
    stop("Length mismatch between categorical_vars and agg_methods_cat!")
  }
}

#' Check if all are NULL
#'
#' Small helper function to check if all of the provided input arguments are NULL.
#'
#' @param ... - Any number of unnamed input arguments
#'
#' @return bool
all_null <- function(...) {
  all(sapply(list(...), is.null))
}

#' Check if all are FALSE
#'
#' Small helper function to check if all of the provided input arguments are FALSE.
#'
#' @param ... - Any number of unnamed input arguments
#'
#' @return bool
all_false <- function(...) {
  all(sapply(unlist(list(...)), isFALSE))
}

#' Check if any is NULL
#'
#' Small helper function to check if any of the provided input arguments is NULL.
#'
#' @param ... - Any number of unnamed input arguments
#'
#' @return bool
any_null <- function(...) {
  any(sapply(list(...), is.null))
}

#' Aggregate a given data.table object based on config
#'
#' Wrapper around the aggregator function to aggregate a given data set with the specified configuration block
#' within the configuration file.
#'
#' @param df data.table::data.table - Object containing the data to be aggregated
#' @param config list - Configuration list from the config file
#' @param ... - Any number of additional named arguments to pass through to the aggregator and in turn to collapse::collap()
#'
#' @return Optional<data.table::data.table|Error>
aggregate_data_from_config <- function(df, config, ...) {
  # Parse all inputs from the config and assign them to temporary variables
  master <- config[["master_col"]]
  variant <- config[["variant_col"]]
  numerical_vars <- config[["numerical_vars"]]
  numerical_methods <- config[["numerical_methods"]]
  weight_vars <- config[["weight_vars"]]
  use_weight <- c(config[["numerical_weights"]], config[["categorical_weights"]])
  categorical_vars <- config[["categorical_vars"]]
  categorical_methods <- config[["categorical_methods"]]
  time_col <- config[["time_col"]]

  # 0) No master or variant specified -> raise an error
  if (all_null(master, variant)) {
    logger::log_fatal("Aggregation failed because of missing identification column (master | variant)")
    stop("Aggregation failed because of missing identification column (master | variant)")
  }
  # 1) Configuration defines Master - Variants variables
  #    => aggregate to the Master level
  if (!any_null(config[["master_col"]], config[["variant_col"]])) {
    logger::log_info("Aggregate input data based on Master-Variants concept to Master level")
    logger::log_debug("Provided data: {as.character(str(df))}")
    logger::log_debug("Provided configuration: {as.character(str(config))}")
    df <- aggregator(df,
                     index_vars = master,
                     numerical_vars = numerical_vars,
                     categorical_vars = categorical_vars,
                     agg_methods_num = numerical_methods,
                     agg_methods_cat = categorical_methods,
                     weight_vars = weight_vars,
                     use_weight = use_weight,
                     ...)
    return(df)
  }

  # 2) Configuration defines Master OR Variant
  #    => Return the input data and don't aggregate, since no multiple levels are defined
  if (any_null(master, variant)) {
    logger::log_debug("Found one leading ID from configuration: {master %||% variant}")
    # => Check if a time column is present, if so aggregate on the given Master or Variant
    if (!is.null(time_col)) {
      logger::log_debug("Found date column - aggregate by {master %||% variant}")
      df <- aggregator(df,
                       index_vars = master %||% variant,
                       numerical_vars = numerical_vars,
                       categorical_vars = categorical_vars,
                       agg_methods_num = numerical_methods,
                       agg_methods_cat = categorical_methods,
                       weight_vars = weight_vars,
                       use_weight = use_weight,
                       ...)
      return(df)
    }
  }
  df
}


#' Categorical aggregator
#'
#' This function takes a data.table object and a vector of categorical columns as input and returns the specified mehod
#' result of each unique column entry relative to the total amount of data entries passed.
#'
#' @param df A data.table object
#' @param cols The categorical columns for frequency caluclations
#' @param method The method passed to \code{frequency_values}
#'
#' @return A data.table with the method result and input columns specified
#'
#' @import data.table
categorical_aggregation <- function(df, cols, method = "freq") {
  lapply(df[, ..cols], frequency_values, method) |>
    collapse::unlist2d(idcols = "col", DT = TRUE)
}

#' Frequency Values function
#'
#' @import data.table
frequency_values <- function(vec, method = NULL) {
  method %||% stop("Please define a method!")
  out <- switch(method,
    "count" = {
      collapse::fcount(vec, name = "f")
    },
    "freq" = {
      tmp <- collapse::fcount(vec)
      within(tmp, { f = N / collapse::fsum(N) })
    },
    "inv_freq" = {
      tmp <- collapse::fcount(vec)
      within(tmp, { f = 1 / (N / collapse::fsum(N)) })
    },
    {
      stop(sprintf("Invalid method %s selected - usable are %s, %s and %s",
                   method, "count", "freq", "inv_freq"))
    }
  )
  collapse::qDT(out[, c("x", "f")])
}

