xx <- rand_ts(m = 20,n = 2)

test_that("rand_ts", {
  expect_type(xx,"double")
  expect_equal(dim(xx),c(20,2))
})

test_that("ann_return",{
  a <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
  expect_equal(ann_return(a,t=12,method="geometric"),0.2338147,tolerance = 1e-7)
  expect_type(ann_return(xx),"double")
})
