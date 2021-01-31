
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


