AA <- rand_ts(m = 20,n = 2,as_ret = T)
xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
yy <- c(-0.005,0.081,0.04,-0.037,-0.061,0.058,-0.049,-0.021,0.062,0.058)
bb <- c(0.002,0.026,0.011,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039,-0.005,0.081,0.04,-0.037,-0.061,
        0.014,-0.049,-0.021,0.062,0.058,-0.064,0.017,-0.004,-0.002,-0.021,0.011,0.047,0.024,0.033,-0.007,0.047,0.006,0.01,-0.002,0.034,0.01)

test_that("rand_ts", {
  expect_type(AA,"double")
  expect_equal(dim(AA),c(20,2))
})

test_that("ann_return",{
  expect_equal(ann_return(xx,t=12,is_geom=TRUE),0.2338147,tolerance = 1e-7)
  expect_type(ann_return(xx),"double")
})


test_that("ann_risk",{
  expect_equal(ann_risk(xx,t=12),0.08047277,tolerance = 1e-7)
  expect_type(ann_risk(xx),"double")
})

test_that("active_return",{
  expect_equal(active_return(xx,yy,t=12),0.08872457,tolerance = 1e-7)
  expect_type(ann_risk(xx),"double")
  expect_type(ann_risk(yy),"double")
  expect_length(active_return(AA[,1],AA[,2]),1L)
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

test_that("adj_ann_sharpe_ratio",{
  expect_equal(adj_ann_sharpe_ratio(xx,t = 12),3.735901,tolerance = 1e-6)
  expect_type(ann_sharpe_ratio(xx),"double")
})

test_that("drawdown",{
  res <- c(0.000,0.000,0.000,0.009,0.000,0.000,0.000,0.000,0.014,0.000)
  expect_equal(drawdown(xx,is_geom = T),res,tolerance = 1e-4)
  expect_type(drawdown(xx),"double")
  expect_length(drawdown(xx),length(res))
})

test_that("drawdown_info",{
  expect_type(drawdown_info(xx),"list")
  expect_length(drawdown_info(xx),7)
})

test_that("avg_drawdown",{
  expect_equal(avg_drawdown(xx),0.0115,tolerance = 1e-4)
  expect_type(ann_sharpe_ratio(xx),"double")
})

test_that("hist_var",{
  expect_equal(hist_var(xx),-0.01175,tolerance = 1e-4)
  expect_type(hist_var(xx),"double")
})

test_that("param_var",{
  expect_equal(param_var(xx),-0.02031075,tolerance = 1e-7)
  expect_type(param_var(xx),"double")
})

test_that("hist_cvar",{
  expect_equal(hist_cvar(xx),-0.014,tolerance = 1e-3)
  expect_type(hist_cvar(xx),"double")
})

test_that("param_cvar",{
  expect_equal(param_cvar(mean(xx),stats::sd(xx)),-0.03001782,tolerance = 1e-6)
  expect_type(param_cvar(mean(xx),stats::sd(xx)),"double")
})



