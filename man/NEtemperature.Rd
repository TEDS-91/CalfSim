% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-energy-requirements.R
\name{NEtemperature}
\alias{NEtemperature}
\title{NASEM (2021) equation to predict the additional maintenance energy necessary
for either heat or cold stress abatement (Mcal/day).}
\usage{
NEtemperature(EBW = 42.3, age = 15, temperature = 12)
}
\arguments{
\item{EBW}{Empty body weight (kg).}

\item{age}{Age (days).}

\item{temperature}{Average temperature (°C).}
}
\value{
The additional maintenance energy for heat or clod stress abatement (Mcal/day).
}
\description{
NASEM (2021) equation to predict the additional maintenance energy necessary
for either heat or cold stress abatement (Mcal/day).
}
\examples{
NEtemperature(EBW = 41.85, age = 1, temperature = 10)
}
