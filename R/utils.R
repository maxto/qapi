# utils

#' As xts
#'
#' Convert vector/matrix/data.frame to xts object. Row names are dates with backward calculation from today.
#'
#' @param x vector/matrix/data.frame/... object
#'
#' @return xts
#' @export
#'
#' @examples
#' as_xts(matrix(NA,10,2))
#' as_xts(stats::rnorm(10))
#' as_xts(data.frame(matrix(NA,10,2)))
#'
as_xts <- function(x) {
  d <- seq(Sys.Date()-NROW(x)+1,Sys.Date(),1)
  out <- xts::xts(x, order.by = d)
  if (is.null(colnames(out))) {
    colnames(out) <- paste0("s",1:NCOL(out))
  }
  out
}



#' Windowing
#'
#' Create a training/test set with sliding windows. Return a list of indices for training and test set
#'
#'
#' @param x number of elements to split as scalar
#' @param k windows size (training)
#' @param s sliding step (test)
#' @param method "window" (default) or "anchored" (no old data forgetting)
#' @param overlapping TRUE, skip training+test windows (e.g. training 1:12/test 13:15,training 16:27/test 28:30,...)
#'
#' @return list
#' @export
#'
#' @examples
#' windowing(60,12,3)
#' windowing(60,12,3,"anchored")
#' windowing(60,12,3,overlapping=TRUE)
#'
windowing <- function(x = NULL,k = NULL,s = 1,method = c("window","anchored"),overlapping=F) {
  if (is.null(x)) stop("insert a number")
  if (is.null(k)) k <- length(x)
  if (s > x) stop("length of window size cannot be higher than length of sample")
  method <- method[1]
  idx <- 1:x
  out <- list("train" = idx,"test" = idx)
  if (x == k) {
    # train == test
    return (out)
  }
  v <- 0
  if (overlapping) {
    v <- k
  }

  nseq <- seq(1,x-k+1,s+v)
  n <- length(nseq)
  train_idx <- vector("list",n)
  test_idx <- vector("list",n)
  t <- 1
  for (j in nseq) {
    if ((j + k -1) < x) {
      switch(tolower(method),
             "window" = train_idx[[t]] <- idx[j:(j+k-1)],
             "anchored" = train_idx[[t]] <- idx[1:(j+k-1)],
             stop("unknown method")
      )
      test_idx[[t]] <- idx[(j + k):min(x,j+k+s-1)]
      t <- t + 1
    } else {
      train_idx[[t]] <- NULL
      test_idx[[t]] <- NULL
    }
  }
  out$train <- train_idx
  out$test <- test_idx
  out
}
