% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa_find_dvid_values.R
\name{find_dvid_values}
\alias{find_dvid_values}
\title{Finds DVID values based on the DVID column name.}
\usage{
find_dvid_values(directory, idv, dvid_name)
}
\arguments{
\item{directory}{A path to the qa run directory.}

\item{idv}{The string of the idv name from the -idv option in the qa run.}

\item{dvid_name}{The string of the idv name from the -idv option in the qa run.}
}
\value{
A numeric vector of all DVID values. If no DVIDs then will be set to string 'NA'.
}
\description{
Finds DVID values based on the DVID column name.
}
