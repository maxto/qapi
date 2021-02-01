#' Random time series
#'
#' Create random time series with normal distribution
#'
#' @param m num of samples
#' @param n num of series
#' @param as_ret TRUE for returns, FALSE for values
#' @param method output class: matrix, data.frame or xts.
#'
#' @return matrix, data.frame or xts
#' @export
#'
#' @examples
#' rand_ts(20,2)
#' rand_ts(20,3,as_ret=TRUE,method="xts")
#'
rand_ts <- function(m = 20,n = 1,as_ret = F,method = c("matrix","xts","data.frame")) {
  method <- method[1]
  ret <- matrix(stats::rnorm(m*n),m,n)/100
  colnames(ret) <- paste0("s",1:n)
  rownames(ret) <- 1:m
  out <- apply(ret,2,function(a)cumprod(1+a))
  if (as_ret) {
    out <- ret
  }
  switch(method,
         "matrix" = as.matrix(out),
         "data.frame" = as.data.frame(out),
         "xts" = as_xts(out),
         stop("unknown method")
  )

}
