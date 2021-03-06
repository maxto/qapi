% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quants.R
\name{sharpe_ratio}
\alias{sharpe_ratio}
\title{Sharpe ratio}
\usage{
sharpe_ratio(x, frisk = 0)
}
\arguments{
\item{x}{asset returns}

\item{frisk}{annual free-risk rate. For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc}
}
\value{
numeric
}
\description{
Sharpe ratio for a vector of asset/portolio returns
}
\examples{
xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
sharpe_ratio(xx,frisk=0.02/12)

}
