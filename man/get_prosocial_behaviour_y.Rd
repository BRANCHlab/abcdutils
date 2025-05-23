% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_prosocial_behaviour_y}
\alias{get_prosocial_behaviour_y}
\title{Youth report of prosocial behaviour}
\usage{
get_prosocial_behaviour_y(ce_y_psb, subjects = NULL, t = NULL, no_zero = FALSE)
}
\arguments{
\item{ce_y_psb}{Youth Prosocial Behavior Survey.}

\item{subjects}{Vector of subjects to filter to.}

\item{t}{timepoint of data collection (0: baseline, 1: 1yfu, ...)}

\item{no_zero}{Boolean indicating if zero values should be replaced with 1}
}
\value{
A data frame containing 3 youth responses concerning prosocial
behaviour.
}
\description{
This function returns youth responses to the ABCD Parent Prosocial Behavior
Survey. The values are stored in the ABCD table \code{ce_y_psb.txt} under the
names \code{prosocial_q1_y}, \code{prosocial_q2_y}, and \code{prosocial_q3_y}.
The function also renames the variables as follows:
\itemize{
\item "prosocial_q1_y" -> "considerate_y"
\item "prosocial_q2_y" -> "helps_hurt_y"
\item "prosocial_q3_y" -> "helpful_y"
}
}
