
test_that("rand_ts", {
  a1 <- rand_ts(m = 20,n = 3,as_ret = T,method = "matrix")
  a2 <- rand_ts(m = 20,n = 3,as_ret = T,method = "data.frame")
  a3 <- rand_ts(m = 20,n = 3,as_ret = T,method = "xts")
  expect_identical(class(a1),c("matrix","array"))
  expect_identical(class(a2),"data.frame")
  expect_identical(class(a3),c("xts","zoo"))
  expect_equal(dim(a1),c(20,3))
  expect_equal(dim(a2),c(20,3))
  expect_equal(dim(a3),c(20,3))
})
