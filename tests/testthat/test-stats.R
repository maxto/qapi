xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)

test_that("skweness", {
  expect_equal(skewness(xx),0.6174813,tolerance = 1e-7)
  expect_type(skewness(xx),"double")
})

test_that("kurtosis",{
  expect_equal(kurtosis(xx),3.037581,tolerance = 1e-7)
  expect_type(kurtosis(xx),"double")
})
