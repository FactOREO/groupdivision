#' Get target data based on a given configuration block
#'
#' Requires a configuration list for a data input. See example configuration.
#'
#' @param config list: Parsed YAML configuration from config::get() as list
#'
#' @return data.table::data.table
#'
#' @import data.table
load_data <- function(config = NULL) {
  config %||% {
    ## Tried to run the data import with NULL object, return NULL invisibly
    logger::log_warning("No configuration object provided - return NULL")
    return(invisible(NULL))
  }
  ## Check if enabled is TRUE
  if (!config[["enabled"]] %||% FALSE) {
    logger::log_info("Will not load data because it is not enabled")
    return(invisible(NULL))
  }
  ## Check if a file is provided and if it exists
  path <- config[["path"]] %||% {
    logger::log_error("No input data file(s) for items configured!")
    stop("No input data file for items configured!")
  }

  ## Check the path for type (dir | file)
  if (file_test("-d", path)) {
    ## if dir, we need pattern
    pattern <- config[["pattern"]] %||% {
      logger::log_error("No pattern provided to find files in {path}!")
      stop(sprintf("No pattern provided to find files in %s!", path))
    }
    target_files <- list.files(path, pattern = pattern, full.names = TRUE)
    logger::log_debug("Found {length(target_files)} data files at `{path}` matching pattern `{pattern}`")
  } else if (file_test("-f", path)) {
    ## if file, not
    target_files <- path
    logger::log_debug("Found data file at {target_files}")
  } else {
    ## if neither, raise error
    logger::log_error("Provided path `{path}` is neither directory nor file!")
    stop(sprintf("Provided path `%s` is neither directory nor file!", path))
  }

  ## Now get the configuration to read the data
  config <- parse_data_config(config)
  logger::log_debug(str(config))

  ## Extract columns and types respectively
  data_vars <- c(
    config[["numerical_vars"]],
    config[["categorical_vars"]],
    config[["master_col"]],
    config[["variant_col"]],
    config[["time_col"]]
  )
  data_types <- c(
    rep("numeric", length(config[["numerical_vars"]])),
    rep("character", length(config[["categorical_vars"]])),
    rep("character", length(config[["master_col"]])),
    rep("character", length(config[["variant_col"]])),
    rep("IDate", length(config[["time_col"]]))
  )
  logger::log_debug("Assigned types: {paste0(data_vars, ' (', data_types, ')', collapse = ' | ')}")
  names(data_types) <- data_vars

  ## Return the target data
  data.table::rbindlist(lapply(
    target_files,
    data.table::fread,
    select = data_vars,
    colClasses = data_types
  ), use.names = TRUE)
}

  # ## Get a look up table if master and variants are specified for later aggregations
  # look_up_table <- NULL
  # master_col <- items_config[["master_col"]]
  # variant_col <- items_config[["variant_col"]]
  # if (!is.null(master_col) && !is.null(variant_col)) {
  #   logger::log_info("Master variant concept specified - define look up table")
  #   look_up_table <- collapse::funique(
  #     items[, .(mC, vC),
  #           env = list(mC = items_config[["master_col"]],
  #                      vC = items_config[["variant_col"]])]
  #   )
  #   data.table::setkeyv(look_up_table, variant_col)
  #   logger::log_debug(as.character(str(look_up_table |> head())))
  # }
  # aggregate_variants <- items_config[["aggregate"]] %||% FALSE
  # if (aggregate_variants) {
  #   logger::log_info("Aggregate items with specified aggregation settings")
  #   items <- aggregator.aggregate_input_data(items, items_config)
  #   logger::log_info("Finished aggregation of items")
  #   logger::log_debug(as.character(str(items |> head())))
  # }
  # list("items" = items, "look_up_table" = look_up_table)
# }
