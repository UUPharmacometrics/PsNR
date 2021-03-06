% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa_get_resmod_structural_dofv.R
\name{get_resmod_structural_dofv}
\alias{get_resmod_structural_dofv}
\title{Get dofv value from the resmod.csv file from the idv_varying_theta model for the specific dvid value.}
\usage{
get_resmod_structural_dofv(directory, idv, dvid = "NA")
}
\arguments{
\item{directory}{A path to the qa run directory.}

\item{idv}{The string of the idv name.}

\item{dvid}{A dvid value. By default dvid='NA'.}
}
\value{
A numeric resmod structural dofv value. If resmod.csv file does not exist will return string "ERROR".
}
\description{
Get dofv value from the resmod.csv file from the idv_varying_theta model for the specific dvid value.
}
