#' Random time series
#'
#' Create random time series with normal distribution
#'
#' @param m num of samples
#' @param n num of series
#' @param as_ret TRUE for returns, FALSE for values
#'
#' @return matrix
#' @export
#'
#' @examples
#' rand_ts(20,2)
#' rand_ts(20,3,as_ret=TRUE)
#'
rand_ts <- function(m = 20,n = 1,as_ret = F) {
  ret <- matrix(stats::rnorm(m*n),m,n)/100
  colnames(ret) <- paste("s",1:n)
  rownames(ret) <- 1:m
  val <- apply(ret,2,function(a)cumprod(1+a))
  if (as_ret) {
    return (ret)
  }
  val
}

#' Annualized return
#'
#' @param x asset/portfolio returns
#' @param t frequencey of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param method "geometric" or "simple" (def: "geometric")
#'
#' @return numeric
#' @export
#'
#' @examples
#' a <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_return(a,t=12,method="geometric")
#'
ann_return <- function(x,t=252,method="geometric") {
  n <- length(x)
  switch(method,
         "geometric" = prod(1+x)^(t/n) - 1,
         "simple" = mean(x) * t,
         stop("unkown method")
  )
}
