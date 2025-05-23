% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbi.R
\name{detail_mtbi}
\alias{detail_mtbi}
\title{Extract mTBI subjects with a minimum time-since-last-mtbi threshold}
\usage{
detail_mtbi(ph_p_otbi, abcd_y_lt, subjects = NULL, t = 0)
}
\arguments{
\item{ph_p_otbi}{ABCD table containing TBI information.}

\item{abcd_y_lt}{Data frame containing age information}

\item{subjects}{Vector of subjects to extract data for.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
subjects Data frame containing list of required subjects
}
\description{
Extract mTBI subjects with a minimum time-since-last-mtbi threshold
}
