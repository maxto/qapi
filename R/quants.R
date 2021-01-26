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
