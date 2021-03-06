% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_ext_file_manipulations.R,
%   R/qa_count_thetas.R
\name{count_thetas}
\alias{count_thetas}
\title{Count number of THETAs.}
\usage{
count_thetas(filename, iteration = -1e+09, keep_na = FALSE)

count_thetas(filename, iteration = -1e+09, keep_na = FALSE)
}
\arguments{
\item{filename}{A string of the ext file name.}

\item{iteration}{A numeric value from the ITERATION column in the ext file. By default iteration=-1000000000.}

\item{keep_na}{A logical indicating whether thetas with missing values should be counted. By default keep_na=FALSE.}
}
\value{
Number of THETAs.

Number of thetas in the ext file.
}
\description{
Count number of THETAs.

Count number of thetas in the ext file.
}
