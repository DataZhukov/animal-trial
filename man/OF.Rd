% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignPens.R
\name{OF}
\alias{OF}
\title{Function to optimize for switchInPen function}
\usage{
OF(x, data, ...)
}
\arguments{
\item{x}{numeric vector of pen numbers}

\item{data}{data.table with column Speen_gew (weaning weights of piglets)}

\item{...}{parameters to be applied to sd and tapply}
}
\value{
numeric
}
\description{
Calculates standard deviation of average weight per pen
}
