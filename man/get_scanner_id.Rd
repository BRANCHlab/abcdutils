% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_scanner_id}
\alias{get_scanner_id}
\title{Extract MRI scanner serial number}
\usage{
get_scanner_id(
  mri_y_adm_info,
  subjects = NULL,
  t = NULL,
  abcd_y_lt = NULL,
  sample_missing = FALSE
)
}
\arguments{
\item{mri_y_adm_info}{Data frame storing MRI administrative information.}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}

\item{abcd_y_lt}{Data frame storing site information.}

\item{sample_missing}{If TRUE, populates missing scanner IDs based on the
scanners present at each observation's site of data collection. Sampling
is weighted according to prevalence of that scanner among all ABCD subjects
with scanner ID values from that site.}
}
\value{
A data frame containing subjectkey and scanner ID.
}
\description{
Extracts variable \code{mri_info_deviceserialnumber} from table \code{mri_y_adm_info}
as \code{scanner_id}.
}
