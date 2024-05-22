## Main script to run the A/B Division
## Load all functions
invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".*\\.[rR]$"), source))

## Prepare the runtime session and get the provided configuration
full_config <- prepare_ab_division_session()
items_config <- full_config[["items"]] %||% list()
transactions_config <- full_config[["transactions"]] %||% list()
logging_config <- full_config[["logging"]] %||% list()

## Set up the global logger
if (logging_config[["enabled"]]) {
  logger::log_threshold(logging_config[["level"]] %||% "INFO")
  log_layout <- logger::layout_glue_generator(
    format = "{format(time, \"%F %T\")} [{ns}: {fn}] {level} {msg}"
  )
  logger::log_layout(log_layout)
  log_path <- logging_config[["path"]]
  log_file <- paste0(log_path, "/abdivision.log")
  if (!dir.exists(log_path)) {
    logger::log_info("Logging directory is non-existing - try to create it")
    dir.create(log_path)
  }
  logger::log_appender(logger::appender_tee(log_file, max_lines = 5e2, max_files = 5L))
}

## Start the AB Division
logger::log_info("\n
 ____ _____  _    ____ _____      _    ____    ____ _____     _____ ____ ___ ___  _   _
/ ___|_   _|/ \\  |  _ \\_   _|    / \\  | __ )  |  _ \\_ _\\ \\   / /_ _/ ___|_ _/ _ \\| \\ | |
\\___ \\ | | / _ \\ | |_) || |     / _ \\ |  _ \\  | | | | | \\ \\ / / | |\\___ \\| | | | |  \\| |
 ___) || |/ ___ \\|  _ < | |    / ___ \\| |_) | | |_| | |  \\ V /  | | ___) | | |_| | |\\  |
|____/ |_/_/   \\_\\_| \\_\\|_|   /_/   \\_\\____/  |____/___|  \\_/  |___|____/___\\___/|_| \\_|
")

## Get the raw data
logger::log_info("Import items data")
items <- load_data(items_config)
logger::log_info("Import transactions")
transactions <- load_data(transactions_config)

## Parse all sub configuration blocks
logger::log_info("Parse items config")
items_config <- parse_data_config(items_config)
logger::log_info("Parse transactions config")
transactions_config <- parse_data_config(transactions_config)
logger::log_info("Get algorithm config")
algorithm_config <- get_strategy_config(full_config[["group_division"]])
selected_strategy <- full_config[["group_division"]][["strategy"]]
logger::log_info("Get output config")
output_config <- full_config[["output"]]

## Create data set to use for A/B-Division
logger::log_info("Create A/B-Division data set from given sources")
df <- create_ab_division_data(items, transactions, items_config, transactions_config)

## Run the chosen algorithm
division <- switch(
  selected_strategy,
  "genetic_algorithm" = run_genetic_algorithm(
    df, algorithm_config,
    numerical_vars = c(items_config[["numerical_vars"]], transactions_config[["numerical_vars"]]),
    categorical_vars = c(items_config[["categorical_vars"]], transactions_config[["categorical_vars"]])
  ),
  {
    logger::log_fatal("Unrecognized algorithm strategy `{selected_strategy}` chosen!")
    stop(sprintf("Unrecognized algorithm strategy `%s` chosen!", selected_strategy))
  }
)

## Save the result
df[, group := division]
data.table::fwrite(df, output_config[["result_file"]], sep = "|")
