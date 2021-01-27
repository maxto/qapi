
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

