test_that("rand_ts", {
  x <- rand_ts(m = 20,n = 2)
  expect_type(x,"double")
  expect_equal(dim(x),c(20,2))
})
