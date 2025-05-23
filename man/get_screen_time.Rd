% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_screen_time}
\alias{get_screen_time}
\title{Extract healthy behaviours: screen time questionnaire}
\usage{
get_screen_time(nt_p_stq, subjects = NULL, t = NULL)
}
\arguments{
\item{nt_p_stq}{ABCD Parent Screen Time Survey.}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}
}
\value{
A data frame containing screen time data.
}
\description{
This function returns the screen time data from the ABCD Parent Screen Time
Survey. The values are stored in the ABCD table \code{nt_p_stq.txt} under the
names \code{screentime1_p_hours}, \code{screentime1_p_minutes}, \code{screentime2_p_hours},
\code{screentime2_p_minutes}, \code{screentime_1_wknd_hrs_p},
\code{screentime_1_wknd_min_p}, \code{screentime_1_wkdy_hrs_p}, and
\code{screentime_1_wkdy_min_p}. The final measures are reported in hours.
}
