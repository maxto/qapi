xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)

test_that("skweness", {
  expect_equal(skewness(xx),0.6174813,tolerance = 1e-7)
  expect_type(skewness(xx),"double")
})

test_that("kurtosis",{
  expect_equal(kurtosis(xx),3.037581,tolerance = 1e-6)
  expect_type(kurtosis(xx),"double")
})

test_that("normpdf",{
  aa <- normpdf(0,mean(xx),sd(xx))
  expect_equal(aa,12.76218,tolerance = 1e-5)
  expect_type(aa,"double")
  expect_length(aa,1L)
})

test_that("norminv",{
  aa <- norminv(0.01,mean(xx),sd(xx))
  expect_equal(aa, -0.0361422,tolerance = 1e-6)
  expect_type(aa,"double")
  expect_length(aa,1L)
})
