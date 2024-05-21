#' Prepare A/B Division Session
#'
#' Wrapper around some utility functions to set up, load and check all
#' requirements of the A/B Division program. Should be run within every
#' run to make sure the provided configuration and options are valid.
#'
#' @return A configuration list for the further use in the program
#'
#' @export
prepare_ab_division_session <- function() {
  cli_opts <- parse_cli_options()
  config <- setup_environment_config(cli_opts$conf)
  prepare_runtime_session()
  config
}

#' Parse CLI options
#'
#' This function is in charge of finding as well as processing all given
#' command line arguments. They are defined within this function for now
#' as well, to keep it simple. This might change in future versions.
#'
#' @return list - List of parsed CLI options
parse_cli_options <- function() {
  cli_options <- list(
    optparse::make_option(
      opt_str = c("-c", "--conf"), type = "character", default = "./conf.yml",
      help = "Provide a path to a configuration file"
    )
  )
  optparse::parse_args(optparse::OptionParser(option_list = cli_options))
}

#' Setup environment configuration
#'
#' This functions loads a given configuration file, validates it and sets up
#' the logging capabilities of the R package {logger}.
#'
#' @param path_to_config String representing a path to a valid configuration file.
#'
#' @return list - A configuration list for the further use in the program
#'
#' @import logger
setup_environment_config <- function(path_to_config) {
  ## Initialize the logger to log everything on STDOUT at the beginning
  ## but enable file logging as well as specific STDOUT logging later
  ## based on the configuraiton file.
  if (!file.exists(path_to_config)) {
    stop(sprintf("Could not find configuration file `%s`.", path_to_config))
  }
  config <- tryCatch(
    expr = {
      logger::log_info("Read configuration from file {path_to_config}")
      config::get(file = path_to_config)
    },
    error = function(e) {
      logger::log_fatal("Fatal error occurred: {conditionMessage(e)}")
      stop(e)
    }
  )
  config
}

#' Prepare A/B Division runtime session
#'
#' This function provides the basic setup functionalities to perorm the A/B
#' division algorithms. This includes checking accessibility of available
#' cores for parallel processing.
#'
#' @return NULL
#'
#' @import logger
prepare_runtime_session <- function() {
  logger::log_info("Prepare runtime session for A/B Division")
  CPUS <- getOption("Ncpus") %||% 1L
  options("Ncpus" = CPUS)
  logger::log_debug("Set number of cores to use to {CPUS}")
  data.table::setDTthreads(CPUS)
  future::plan(strategy = "multicore", workers = CPUS)
  logger::log_debug("Cores set by getDTthreads: {data.table::getDTthreads()}")
}
