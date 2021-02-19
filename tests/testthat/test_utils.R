
test_that("as_xts", {
  a1 <- stats::rnorm(10)
  a2 <- data.frame(matrix(NA,10,2))
  a3 <- matrix(0,10,2)
  expect_identical(class(as_xts(a1)),c("xts","zoo"))
  expect_identical(class(as_xts(a2)),c("xts","zoo"))
  expect_identical(class(as_xts(a3)),c("xts","zoo"))
  expect_equal(dim(as_xts(a1)),c(10,1))
  expect_equal(dim(as_xts(a2)),c(10,2))
  expect_equal(dim(as_xts(a3)),c(10,2))
})

test_that("windowing", {
  ww <- windowing(x=60,k=12,s=3,method="window",overlapping = F)
  expect_identical(class(ww),"list")
  expect_length(ww,2)
  expect_equal(names(ww),c("train","test"))
  expect_length(ww$train,16)
  expect_length(ww$test,16)
})
