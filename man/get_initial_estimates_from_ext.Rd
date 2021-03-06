% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_get_initial_estimates_from_ext.R
\name{get_initial_estimates_from_ext}
\alias{get_initial_estimates_from_ext}
\title{Get initial estimates from the ext file.}
\usage{
get_initial_estimates_from_ext(
  filename,
  select = "all",
  iteration = -1e+09,
  do.stop = TRUE
)
}
\arguments{
\item{filename}{A string of the ext file name.}

\item{select}{A string of the initial estimates to get. Possible values "omega","theta","sigma" or "all"(means all three).}

\item{iteration}{A numeric value from the ITERATION column in the ext file. By default iteration=-1000000000.}

\item{do.stop}{A logical indicating whether function should stop if input of the select argument is wrong. By default do.stop=TRUE.}
}
\value{
A data frame of initial estimates.
}
\description{
Get initial estimates from the ext file.
}
