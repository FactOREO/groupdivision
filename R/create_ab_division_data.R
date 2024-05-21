#' Take transactions and items to create final data
#'
#' Given the items and transactions data as well as their respective config blocks,
#' this function creates the final data set to use within the AB Division algorithm
#' chosen.
#'
#' @param items data.table::data.table - Object containing the items data
#' @param transactions data.table::data.table - Object containing the transactions data
#' @param items_config list - Configuration list
#' @param transactions_config list - Configuration list
#'
#' @return Optional<data.table::data.table|error>
create_ab_division_data <- function(items, transactions, items_config, transactions_config) {
  iM <- items_config[["master_col"]] %||% NULL
  iV <- items_config[["variant_col"]] %||% NULL
  tM <- transactions_config[["master_col"]] %||% NULL
  tV <- transactions_config[["variant_col"]] %||% NULL
  if (!any_null(iM, tM)) {
    ## Master is in transactions and items present
    ## => aggregate both data sets (if required) and merge on master
    logger::log_info("Found master in both data sets")
    transactions <- aggregate_data_from_config(transactions, transactions_config)
    items <- aggregate_data_from_config(items, items_config)
    join_cols <- iM
    names(join_cols) <- tM
    df <- collapse::join(items, transactions, on = join_cols, how = "inner", verbose = 0)
  } else if (!any_null(iM, iV, tV)) {
    ## Master - Variant in items present, transactions have variants
    ## => Add masters to transactions, aggregate and merge on master
    join_cols <- iV
    names(join_cols) <- tV
    logger::log_info("Add master column via join on {names(join_cols)} == {join_cols}")
    transactions <- collapse::join(transactions, items[, .(m, v), env = list(m = iM, v = iV)], on = join_cols, how = "inner", verbose = 0)
    # Update the transactions config for the new primary key
    transactions_config[["variant_col"]] <- iM
    logger::log_debug("Aggregate transactions with \n {as.character(str(transactions_config))}")
    transactions <- aggregate_data_from_config(transactions, transactions_config)
    logger::log_debug("Aggregate items with \n {as.character(str(items_config))}")
    items <- aggregate_data_from_config(items, items_config)
    df <- collapse::join(items, transactions, on = iM, how = "inner", verbose = 0)
  } else if (!any_null(iV, tV)) {
    ## Variants are present in both
    logger::log_info("Only variants are present in both data sets")
    join_cols <- iV
    names(join_cols) <- tV
    df <- collapse::join(transactions, items, on = join_cols, how = "inner", verbose = 0)
  } else if (any_null(items, transactions) && !all_null(items, transactions)) {
    ## Only one data set is present
    logger::log_info("Only one data set provided")
    if (!is.null(items)) {
      logger::log_info("Aggregate items")
      df <- aggregate_data_from_config(items, items_config)
    } else {
      logger::log_info("Aggregate transactions")
      df <- aggregate_data_from_config(transactions, transactions_config)
    }
  } else {
    ## Unconsidered case(s) matched - throw error
    logger::log_fatal("Unconsidered case present - readjust config or add another case")
    stop("Unconsidered case present - readjust config or add another case")
  }
  df
}
