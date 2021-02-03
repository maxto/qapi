
#' Skewness
#'
#' @param x numeric vector
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' skewness(xx)
#'
skewness <- function(x) {
  n <- length(x)
  (sum((x-mean(x))^3)/n)/(sum((x-mean(x))^2)/n)^(3/2)
}


#' Kurtosis
#'
#' @param x numeric vector
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' kurtosis(xx)
#'
kurtosis <- function(x) {
  n <- length(x)
  n*sum( (x-mean(x))^4 )/(sum( (x-mean(x))^2 )^2)
}

#' Inverse of the normal cumulative distribution function (cdf)
#'
#' Returns the inverse cdf for the normal distribution with mean MU and standard deviation SIGMA at P value
#'
#' @param p probability value in range 0-1
#' @param mu mean value
#' @param sigma standard deviation
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' norminv(0.01,mean(xx),sd(xx))
#'
norminv <- function(p,mu=0,sigma=1) {
  x0 <- -sqrt(2)*pracma::erfcinv(2*p)
  x0*sigma+mu
}

#' Normal probability density function (pdf)
#'
#' Returns the pdf of the normal distribution with mean MU and standard deviation SIGMA
#' evaluated at the values in X
#'
#' @param x real value
#' @param mu mean value
#' @param sigma standard deviation
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' normpdf(0.01,mean(xx),sd(xx))
#'
normpdf <- function(x,mu=0,sigma=1) {
  exp(-0.5 * ((x - mu)/sigma)^2) / (sqrt(2 * pi) * sigma)
}
