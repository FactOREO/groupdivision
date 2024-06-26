% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_selection.R
\name{steady_state_selection}
\alias{steady_state_selection}
\title{Steady state selection}
\usage{
steady_state_selection(fitness_values, num_parents, seed = NULL)
}
\arguments{
\item{fitness_values}{Vec\if{html}{\out{<numeric>}} - A vector of numeric values representing the individuals fitness values}

\item{num_parents}{integer - The number of individuals to choose}
}
\value{
vec\if{html}{\out{<integer>}}
}
\description{
This selection method will always return the top n individuals from a given vector
of fitness values provided.
}
