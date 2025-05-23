% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbi.R
\name{rename_tbi}
\alias{rename_tbi}
\title{Rename non-descriptive columns in ABCD's Ohio TBI table.}
\usage{
rename_tbi(tbi_df)
}
\arguments{
\item{tbi_df}{A data frame containing columns from abcd_otbi01.txt.}
}
\value{
A modified form of tbi_df with clearer column names.
}
\description{
This function renames columns of ABCD's Ohio TBI Screen data to
be more understandable. Use \code{original_otbi_names()} to display a
conversion table of old and new column names.
}
