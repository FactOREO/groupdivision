#' Get target data based on a given configuration block
#'
#' Requires a configuration list for a data input. See example configuration.
#'
#' @param config list: Parsed YAML configuration from config::get() as list
#'
#' @return data.table::data.table
#'
#' @import data.table
#' @import logger
#' @export
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
