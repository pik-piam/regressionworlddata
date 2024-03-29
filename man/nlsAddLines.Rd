% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlsAddLines.R
\name{nlsAddLines}
\alias{nlsAddLines}
\title{nlsAddLines}
\usage{
nlsAddLines(
  y,
  x,
  countries = 1:5,
  weight = NULL,
  x_log10 = FALSE,
  colors = "black",
  labels = TRUE
)
}
\arguments{
\item{y}{magpie object with y values}

\item{x}{magpie object with x values}

\item{countries}{Choice of countries}

\item{weight}{magpie object with weight}

\item{x_log10}{same as in nlsregression}

\item{colors}{colors of the lines}

\item{labels}{If TRUE, the region, staryear and endyear will be plotted to each line.}
}
\value{
vector with ISO-countrycodes
}
\description{
Adds lines of specific countries into the plot of the function nlsregression. 
nlsregression has to be based on magpie objects for x,y,weight
}
\examples{

\dontrun{ 
data(population_magpie)
nlsregression(y=population_magpie[,,1],x=population_magpie[,,2],
weight = population_magpie[,,1],func = y~a*x+b)
nlsAddLines(y=population_magpie[,,1],x=population_magpie[,,2],
weight = population_magpie[,,1],countries=1:3,colors=1:3)
}
}
\seealso{
\code{\link{nlsregression}}
}
\author{
Benjamin Leon Bodirsky
}
