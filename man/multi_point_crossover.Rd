% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ga_crossover.R
\name{multi_point_crossover}
\alias{multi_point_crossover}
\title{Multiple Points Crossover}
\usage{
multi_point_crossover(parent1, parent2, k, seed = NULL)
}
\arguments{
\item{parent1}{vec\if{html}{\out{<Some>}} - Vector containing the genome information of parent 1}

\item{parent2}{vec\if{html}{\out{<Some>}} - Vector containing the genome information of parent 2}

\item{k}{integer - The number of crossover points}

\item{seed}{Optional<NULL|integer> - A random seed for deterministic outcomes}
}
\value{
Optional<error|list<vec\if{html}{\out{<bool>}},vec\if{html}{\out{<bool>}}>>
}
\description{
This functions performs a multiple points crossover from to given parents and returns two new children.
The \code{k} crossover points are chosen randomly, but can be mande deterministic by providing a seed value.
Only usable in the context of a bivariat target space of valid genes for combination.
}
