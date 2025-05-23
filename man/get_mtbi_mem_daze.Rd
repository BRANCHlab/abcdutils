% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_mtbi_mem_daze}
\alias{get_mtbi_mem_daze}
\title{Extract memory loss / dazed status of latest mTBI}
\usage{
get_mtbi_mem_daze(ph_p_otbi, abcd_y_lt, subjects = NULL, t = NULL)
}
\arguments{
\item{ph_p_otbi}{ABCD table containing TBI information.}

\item{abcd_y_lt}{ABCD table containing age information.}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
A data frame containing latest_mtbi_mem_daze.
}
\description{
Extract memory loss / dazed status of latest mTBI
}
