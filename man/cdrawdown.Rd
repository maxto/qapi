% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quants.R
\name{cdrawdown}
\alias{cdrawdown}
\title{Continous drawdawn}
\usage{
cdrawdown(x, is_geom = TRUE)
}
\arguments{
\item{x}{asset returns}

\item{is_geom}{TRUE geometric, FALSE simple}
}
\value{
numeric
}
\description{
Return largest individual drawdowns
}
\examples{
xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
cdrawdown(xx)

}
