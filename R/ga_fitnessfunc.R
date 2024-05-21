# This module contains the objective function to use for the calculation of a fitness score,
# provided an individual and its genome representation.

#' Calculate fitness score
#'
#' TODO DESCRIPTION
#'
#' @param individual vec<Some> - Vector containing the genome representation of the individual
#' @param df data.table::data.table - Object containing all the (prepared and aggregated) data of the individual
#' @param numerical_vars vec<character> - Vector containing all column names of numerical columns for the calculation
#' @param categorical_columns vec<character> - Vector containing all column names of categorical columns for the calculation
#'
#' @return numeric - Fitness score value of the individual
#'
#' @import collapse
#' @import data.table
calc_fitness_score <- function(individual, df, categorical_vars = NULL, numerical_vars = NULL) {
  dt <- df
  dt[, group := individual]
  dt <- prepare_distance_calculation(dt, "group", categorical_vars, numerical_vars) |>
    collapse::fselect(-1) |>
    t()
  -1 * calculate_distance(dt)
}

#' Distance function
#'
#' This function calculates the distance between n groups within a given individual and its given attributes,
#' e.g. data values from the specified numerical and/or categorical variables within the configuration.
#' If there are only two groups we rely on philentropy::euclidean to do the calculations, else it is run via
#' philentropy::distance obtaining a distance object. This distance object, possibly containing very large difference
#' between some and smaller distances between other groups, is condensed into a single "mean distance" value,
#' calculated as the Root Mean Squared (RMS) Distance.
#'
#' @param X matrix - A matrix containing the distributions of a group per row (!)
#'
#' @return numeric - Exact (if nrows(X) = 2) or weighted mean distance between all passed groups
#'
#' @import philentropy
calculate_distance <- function(X) {
  if (nrow(X) > 2) {
    d <- philentropy::distance(X, method = "euclidean", test.na = FALSE, as.dist.obj = TRUE, mute.message = TRUE) |>
      as.vector()
    return(sqrt(sum(d^2) / nrow(X)))
  }
  philentropy::euclidean(X[1L, ], X[2L, ], FALSE)
}

#' Prepare distance calculation
#'
#' This function is responsible of preparing the given data w.r.t. the group division within a given individual.
#'
#' @param df data.table::data.table - Object containing the numerical and categorical data of the individual
#' @param index_column character - Column name of the index column
#' @param categorical_vars vec<character> - Character vector of column names indicating categorical variables
#' @param numerical_vars vec<character> - Character vector of column names indicating numerical variables
#'
#' @import data.table
#' @import collapse
prepare_distance_calculation <- function(
    df,
    index_column,
    categorical_vars = NULL,
    numerical_vars = NULL) {
  #--- Categorical Data
  if (!is.null(categorical_vars)) {
    cat_out <- collapse::rsplit(df, create_formula(index_column)) |>
      collapse::rapply2d(categorical_aggregation,
                         categorical_vars,
                         method = "freq",
                         classes = "data.table") |>
      collapse::unlist2d(DT = TRUE, idcols = "group") |>
      collapse::fmutate(col = paste(col, x, sep = "_")) |>
      data.table::dcast.data.table(group ~ col, value.var = "f")
    ### Replace NA frequency with 0
    for (j in seq_len(ncol(cat_out))) {
      data.table::set(cat_out, which(is.na(cat_out[[j]])), j, 0)
    }
  }
  #--- Numerical Data
  if (!is.null(numerical_vars)) {
    num_out <- aggregator(df,
                          index_column,
                          numerical_vars    = numerical_vars,
                          agg_methods_num   = rep("fsum", length(numerical_vars)),
                          mc.cores          = getOption("Ncpus"),
                          parallel          = TRUE)
    num_out[, idx := as.character(idx), env = list(idx = index_column)]
  }
  #--- output
  if (exists("cat_out") && exists("num_out")) {
    data.table::merge.data.table(cat_out, num_out, all = TRUE, by = "group") |>
      data.table::melt.data.table(id.vars = index_column) |>
      data.table::dcast.data.table(variable ~ group)
  } else if (exists("cat_out")) {
    data.table::melt.data.table(cat_out, id.vars = index_column) |>
      data.table::dcast.data.table(variable ~ group)
  } else {
    data.table::melt.data.table(num_out, id.vars = index_column) |>
      data.table::dcast.data.table(variable ~ group)
  }
}
