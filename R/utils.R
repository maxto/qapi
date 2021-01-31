# utils

na_omit <- function(x) {
  stats::na.omit(x)
}

as_xts <- function(x) {
  d <- seq(Sys.Date()-NROW(x)+1,Sys.Date(),1)
  xts::xts(x, order.by = d)
}

as_df <- function(x) {
  as.data.frame(x)
}


norminv <- function(p,mu=0,sigma=1) {
  x0 <- -sqrt(2)*pracma::erfcinv(2*p)
  x0*sigma+mu
}

normpdf <- function(x,mu=0,sigma=1) {
  exp(-0.5 * ((x - mu)/sigma)^2) / (sqrt(2 * pi) * sigma)
}
