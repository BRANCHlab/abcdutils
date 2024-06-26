% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_cbcl_aggressive}
\alias{get_cbcl_aggressive}
\title{Get CBCL aggressive data}
\usage{
get_cbcl_aggressive(
  mh_p_cbcl,
  subjects = NULL,
  t = NULL,
  raw = TRUE,
  aggressive_thresh_borderline = 11,
  aggressive_thresh_clinical = 15
)
}
\arguments{
\item{mh_p_cbcl}{NDA cbcl dataframe}

\item{subjects}{Vector of subjectkeys.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}

\item{raw}{Logical value indicating if raw or borderline/clinical
thresholded values should be obtained}

\item{aggressive_thresh_borderline}{threshold for borderline clinical}

\item{aggressive_thresh_clinical}{threshold for clinical}
}
\value{
cbcl_aggressive_r aggressive data
}
\description{
Get CBCL aggressive data
}
