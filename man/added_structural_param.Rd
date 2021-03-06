% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa_added_structural_param.R
\name{added_structural_param}
\alias{added_structural_param}
\title{Get number of added parameters to the idv_varying_theta model in the resmod_results.csv file.}
\usage{
added_structural_param(directory, idv, dvid = "NA")
}
\arguments{
\item{directory}{A path to the qa run directory.}

\item{idv}{A string vector of all possible idv names for the specific qa run.}

\item{dvid}{A dvid value. By default="NA"}
}
\value{
A numeric value of added parameters to the idv_varying_theta model.
If file resmod_results.csv does not exist or all parameter values are missing then added_param will be set to empty character.
}
\description{
Get number of added parameters to the idv_varying_theta model in the resmod_results.csv file.
}
