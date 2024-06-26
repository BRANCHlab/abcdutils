% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{flex_cond}
\alias{flex_cond}
\title{Flexible conditional statement evaluation}
\usage{
flex_cond(x)
}
\arguments{
\item{x}{a conditional expression}
}
\description{
If a conditional statement would have returns an error or 0-length vector,
this function returns FALSE instead. Otherwise, evaluates the expression
normally.
}
