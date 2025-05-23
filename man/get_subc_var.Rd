% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_subc_var}
\alias{get_subc_var}
\title{Extract cortical temporal variances}
\usage{
get_subc_var(mrirstv02, subjects = NULL, t = NULL)
}
\arguments{
\item{mrirstv02}{Data file containing neurite density data}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
A data frame of white matter neurite densities
}
\description{
Extract cortical temporal variances
}
