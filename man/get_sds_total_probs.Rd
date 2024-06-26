% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_sds_total_probs}
\alias{get_sds_total_probs}
\title{General factor for sleep disturbance scale}
\usage{
get_sds_total_probs(ph_p_sds, subjects = NULL, t = NULL)
}
\arguments{
\item{ph_p_sds}{Dataframe containing sleep disturbance scale data.}

\item{subjects}{Vector of subjectkeys.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
full_sleep_df Dataframe containing sleep data
}
\description{
Factor loadings based on Mancini et al., 2019
}
