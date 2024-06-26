% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_crossover.R
\name{single_point_crossover}
\alias{single_point_crossover}
\title{Single Point Crossover}
\usage{
single_point_crossover(parent1, parent2, seed = NULL)
}
\arguments{
\item{parent1}{vec\if{html}{\out{<Some>}} - Vector containing the genome information of parent 1}

\item{parent2}{vec\if{html}{\out{<Some>}} - Vector containing the genome information of parent 2}

\item{seed}{Optional<NULL|integer> - A random seed for deterministic outcomes}
}
\value{
Optional<error|list<vec\if{html}{\out{<bool>}},vec\if{html}{\out{<bool>}}>>
}
\description{
This functions performs a single point crossover from to given parents and returns two new children.
The crossover point is chosen randomly, but can be mande deterministic by providing a seed value.
}
