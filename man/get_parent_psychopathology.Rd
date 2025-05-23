% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_parent_psychopathology}
\alias{get_parent_psychopathology}
\title{Extract parent psychopathology}
\usage{
get_parent_psychopathology(mh_p_asr, subjects = NULL, t = NULL, raw = TRUE)
}
\arguments{
\item{mh_p_asr}{ABCD Parent Adult Self Report Scores Aseba}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}

\item{raw}{Boolean indicating if extracted data should be raw (TRUE) or
t-scores (FALSE). Defaults to TRUE.}
}
\value{
A data frame containing parent psychopathology data.
}
\description{
Extract parent psychopathology
}
