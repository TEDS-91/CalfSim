% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-energy-requirements.R
\name{empty_body_weight}
\alias{empty_body_weight}
\title{NASEM (2021) equation to predict the calf empty body weight (kg) - Table 10-1.}
\usage{
empty_body_weight(BW = 45, liquid_diet_only = TRUE, weaned = FALSE)
}
\arguments{
\item{BW}{Body weight (kg).}

\item{liquid_diet_only}{Argument to define whether the calves have only milk available or also forage.}

\item{weaned}{Argument to define whether the calves are weaned or not.}
}
\value{
The Empty body weight (kg).
}
\description{
NASEM (2021) equation to predict the calf empty body weight (kg) - Table 10-1.
}
\examples{
empty_body_weight(BW = 45, liquid_diet_only = TRUE, weaned = FALSE)
}
