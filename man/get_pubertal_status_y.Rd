% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_pubertal_status_y}
\alias{get_pubertal_status_y}
\title{Youth report of pubertal status}
\usage{
get_pubertal_status_y(ph_y_pds, subjects = NULL, t = NULL, max_value = NULL)
}
\arguments{
\item{ph_y_pds}{Data frame containing youth pubertal status report}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}

\item{max_value}{Maximum value for pubertal status}
}
\value{
A data frame containing average pubertal status
}
\description{
Youth report of pubertal status
}
