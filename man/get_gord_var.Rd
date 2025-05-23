% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_gord_var}
\alias{get_gord_var}
\title{Extract cortical temporal variances}
\usage{
get_gord_var(mri_y_rsfmr_var_gp, subjects = NULL, t = NULL)
}
\arguments{
\item{mri_y_rsfmr_var_gp}{Data file containing neurite density data}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
A data frame of white matter neurite densities
}
\description{
Extract cortical temporal variances
}
