% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_run.R
\name{run_genetic_algorithm}
\alias{run_genetic_algorithm}
\title{Execute the genetic algorithm}
\usage{
run_genetic_algorithm(
  input_data,
  config,
  valid_genes = NULL,
  numerical_vars = NULL,
  categorical_vars = NULL
)
}
\arguments{
\item{config}{list - A list containing all relevant parameters for the genetic algorithm to run (see example  config)}

\item{numerical_vars}{Optional<vec\if{html}{\out{<character>}}|NULL> - A vector of column names to be considered numeric}

\item{categorical_vars}{Optional<vec\if{html}{\out{<character>}}|NULL> - A vector of column names to be considered categorical}

\item{df}{data.table::data.table - Object containing the data to be splitted into groups}
}
\value{
Optional<numeric|error>
}
\description{
This function uses a given data set and it's corresponding values from a given configuration list
to optimally reallocate the given groups w.r.t. to all variables to be considered.
This function is capable of utilizing numerical as well as categorical variables by aggregating them
to obtain comparable matrices.
}
