% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolRegressionTable.R
\name{toolRegressionTable}
\alias{toolRegressionTable}
\title{toolRegressionTable}
\usage{
toolRegressionTable(
  scenario = "SSP2",
  x = "IHME_USD05_PPP_pc",
  denominator = NA,
  z = NA,
  regression_database_file = "scenario_database_regressionworlddata.csv",
  quotient = "pop",
  start_1 = NA,
  start_2 = NA,
  start_3 = NA,
  start_4 = NA,
  start_5 = NA,
  start_6 = NA,
  return_value = FALSE
)
}
\arguments{
\item{scenario}{vector. Default "SSP2". Can be "SSP1", "SSP2", "SSP3", "SSP4", "SSP5" or "mix" and describes the overall scenario of the projection.}

\item{x}{Indep Var}

\item{denominator}{vector. Default NA. Specific fooddenominator share to make projection for.}

\item{z}{other independent variables}

\item{regression_database_file}{file with regressions to calculate}

\item{quotient}{vector. Default is population ("pop")}

\item{start_1}{Default NA. Startvalue for 1st parameter.}

\item{start_2}{Default NA. Startvalue for 2nd parameter.}

\item{start_3}{Default NA. Startvalue for 3rd parameter.}

\item{start_4}{Default NA. Startvalue for 4th parameter.}

\item{start_5}{Default NA. Startvalue for 5th parameter.}

\item{start_6}{Default NA. Startvalue for 6th parameter.}

\item{return_value}{Default to False. This is to stop printing the updated dataset on console. If you'd like to keep the updated dataset as an object, set this to true.}
}
\value{
data frame with additional rows containing parameters of newly calculated regression.
}
\description{
creates Regression for selected options and saves calculated parametes inside the table.
}
\seealso{
\code{\link{toolRegression}}
}
\author{
Abhijeet Mishra, Eleonora Martinelli
}
