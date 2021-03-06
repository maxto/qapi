% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quants.R
\name{hist_cvar}
\alias{hist_cvar}
\title{Historical Conditional Value At Risk}
\usage{
hist_cvar(x, p = 0.95)
}
\arguments{
\item{x}{asset returns}

\item{p}{confidence level}
}
\value{
numeric
}
\description{
Historical conditional value at risk
}
\examples{
xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
hist_cvar(xx,p=0.95)

}
