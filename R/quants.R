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
#' Average annualized returns
#'
#' @param x asset returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param method "geometric" or "simple" (def: "geometric")
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_return(xx,t=12,method="geometric")
#'
ann_return <- function(x,t=252,method="geometric") {
  n <- length(x)
  switch(method,
         "geometric" = prod(1+x)^(t/n) - 1,
         "simple" = mean(x) * t,
         stop("unkown method")
  )
}

#' Annualized Risk
#'
#' Annualized standard deviation
#'
#' @param x asset returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_risk(xx,t=12)
#'
ann_risk <- function(x,t=252) {
  stats::sd(x) * sqrt(t)
}



#' Active return
#'
#' Asset annualized return minus Benchmark annualized return
#'
#' @param x asset returns
#' @param y benchmark returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param method "geometric" or "simple" (def: "geometric")
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' yy <- c(0.04,-0.022,0.043,0.028,-0.078,-0.011,0.033,-0.049,0.09,0.087)
#' active_return(xx,yy,t=12)
#'
active_return <- function(x,y,t=252,method="geometric") {
  ann_return(x,t,method) - ann_return(y,t,method)

}


#' Sharpe ratio
#'
#' Sharpe ratio for a vector of asset/portolio returns
#'
#' @param x asset returns
#' @param frisk annual free-risk rate (def: 0). For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' sharpe_ratio(xx,frisk=0.02/12)
#'
sharpe_ratio <- function(x,frisk=0) {
  (mean(x) - frisk)/stats::sd(x)
}


#' Excess return
#'
#' Return on asset - risk free rate
#'
#' @param x asset returns
#' @param frisk annual free-risk rate (def: 0). For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' excess_return(xx,frisk=0.02/12)
#'
excess_return <- function(x,frisk=0) {
  x - rep(frisk,1,length(x))
}


#' Annualized Sharpe ratio
#'
#' Sharpe ratio calculated on annualized excess return / annualized standard deviation
#'
#' @param x asset returns
#' @param frisk annual free-risk rate (def: 0). For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param method method "geometric" or "simple" (def: "geometric")
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_sharpe_ratio(xx,frisk=0.02/12,t=12,method="geometric")
#'
ann_sharpe_ratio <- function(x,frisk=0,t=252,method="geometric") {
  xs <- excess_return(x,frisk)
  ann_return(xs,t,method)/ann_risk(x,t)
}


