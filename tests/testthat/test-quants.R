A <- rand_ts(m = 20,n = 2,as_ret = T)
xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
yy <- c(0.04,-0.022,0.043,0.028,-0.078,-0.011,0.033,-0.049,0.09,0.087)

test_that("rand_ts", {
  expect_type(A,"double")
  expect_equal(dim(A),c(20,2))
})

test_that("ann_return",{
  expect_equal(ann_return(xx,t=12,method="geometric"),0.2338147,tolerance = 1e-7)
  expect_type(ann_return(xx),"double")
  expect_error(ann_return(xx,method = "armonic"))
})


test_that("ann_risk",{
  expect_equal(ann_risk(xx,t=12),0.08047277,tolerance = 1e-7)
  expect_type(ann_risk(xx),"double")
})

test_that("active_return",{
  expect_equal(active_return(xx,yy,t=12),0.04197881,tolerance = 1e-7)
  expect_type(ann_risk(xx),"double")
  expect_type(ann_risk(yy),"double")
  expect_length(active_return(A[,1],A[,2]),1L)
})

test_that("sharpe_ratio",{
  expect_equal(sharpe_ratio(xx),0.7705391,tolerance = 1e-7)
  expect_type(sharpe_ratio(xx),"double")
})

test_that("excess_return",{
  res <- c(0.001333333,0.024333333,0.013333333,-0.010666667,0.012333333,0.022333333,0.013333333,0.064333333,-0.015666667,0.037333333)
  expect_equal(excess_return(xx,frisk = 0.02/12),res,tolerance = 1e-7)
  expect_type(excess_return(xx),"double")
  expect_length(excess_return(xx,frisk = 0.02/12),length(res))
})

test_that("ann_sharpe_ratio",{
  expect_equal(ann_sharpe_ratio(xx,t = 12),2.905513,tolerance = 1e-7)
  expect_type(ann_sharpe_ratio(xx),"double")
})
