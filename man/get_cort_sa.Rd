% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extraction.R
\name{get_cort_sa}
\alias{get_cort_sa}
\title{Extract cortical surface areas}
\usage{
get_cort_sa(mri_y_smr_area_dsk, subjects = NULL, t = NULL)
}
\arguments{
\item{mri_y_smr_area_dsk}{Data file containing cortical data}

\item{subjects}{Dataframe containing list of required subjects}

\item{t}{timepoint for data collection (0: baseline, 1: 1yfu, ... 3: 3yfu)}
}
\value{
cort_sa_df Dataframe of cortical surface areas
}
\description{
Extract cortical surface areas
}
