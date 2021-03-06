% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa_read_results.R
\name{retrieve_qa_results}
\alias{retrieve_qa_results}
\title{Read in QA results}
\usage{
retrieve_qa_results(
  path,
  model_filename,
  psn_options,
  settings = qa_settings()
)
}
\arguments{
\item{path}{Path of the QA run directory}

\item{model_filename}{Name of the model file QA was run on}

\item{psn_options}{List of options provided by PSN}

\item{settings}{Settings}
}
\value{
List
}
\description{
The function is the central place for collecting all QA results. It uses the provided path to the QA directory
to read-in all necessary information and return them as a nested list structure.
}
