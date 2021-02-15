AA <- rand_ts(m = 20,n = 2,as_ret = T)
xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
yy <- c(-0.005,0.081,0.04,-0.037,-0.061,0.058,-0.049,-0.021,0.062,0.058)
ww <- c(0.003,0.026,0.011,-0.01,0.015,0.025,0.016,0.067,-0.014,0.04,-0.005,0.081,0.04,-0.037,-0.061,0.017,-0.049,-0.022,0.07,0.058,-0.065,0.024,-0.005,-0.009)

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
  expect_type(adj_ann_sharpe_ratio(xx),"double")
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

test_that("cdrawdown",{
  expect_equal(cdrawdown(ww),c(0.010000,0.014000,0.005000,0.095743,0.069922,0.065000,0.013955),tolerance = 1e-4)
  expect_type(cdrawdown(ww),"double")
})

test_that("avg_drawdown",{
  expect_equal(avg_drawdown(ww),0.04341824,tolerance = 1e-7)
  expect_type(avg_drawdown(ww),"double")
})

test_that("max_drawdown",{
  expect_equal(max_drawdown(ww),0.144673,tolerance = 1e-6)
  expect_type(max_drawdown(ww),"double")
})

test_that("ulcer_index",{
  expect_equal(ulcer_index(ww),0.06118429,tolerance = 1e-7)
  expect_type(ulcer_index(ww),"double")
})

test_that("calmar_ratio",{
  expect_equal(calmar_ratio(ww,t=12),0.7166391,tolerance = 1e-7)
  expect_type(calmar_ratio(ww,t=12),"double")
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

test_that("downside_risk",{
  expect_equal(downside_risk(xx),0.005263079,tolerance = 1e-7)
  expect_type(downside_risk(xx),"double")
})

test_that("downside_potential",{
  expect_equal(downside_potential(xx), 0.0023,tolerance = 1e-3)
  expect_type(downside_potential(xx),"double")
})

test_that("sortino_ratio",{
  expect_equal(sortino_ratio(xx),3.401051,tolerance = 1e-6)
  expect_type(sortino_ratio(xx),"double")
})

test_that("ann_sortino_ratio",{
  expect_equal(ann_sortino_ratio(xx,t = 12,ann_mar = 0.01),11.48044,tolerance = 1e-5)
  expect_type(ann_sortino_ratio(xx),"double")
})

test_that("upside_risk",{
  expect_equal(upside_risk(xx),0.02789982,tolerance = 1e-7)
  expect_type(upside_risk(xx),"double")
})

test_that("upside_potential",{
  expect_equal(upside_potential(xx),0.0202,tolerance = 1e-4)
  expect_type(upside_potential(xx),"double")
})

test_that("omega_ratio",{
  expect_equal(omega_ratio(ww),1.779783,tolerance = 1e-6)
  expect_type(omega_ratio(ww),"double")
})

test_that("martin_ratio",{
  expect_equal(martin_ratio(ww,t=12),1.694525,tolerance = 1e-6)
  expect_type(martin_ratio(ww,t=12),"double")
})

test_that("pain_index",{
  expect_equal(pain_index(ww),0.03998969,tolerance = 1e-7)
  expect_type(pain_index(ww),"double")
})

test_that("pain_ratio",{
  expect_equal(pain_ratio(ww,t = 12),2.592625,tolerance = 1e-6)
  expect_type(pain_ratio(ww,t = 12),"double")
})

test_that("burke_ratio",{
  expect_equal(burke_ratio(ww,t= 12),0.756221,tolerance = 1e-6)
  expect_type(burke_ratio(ww,t= 12),"double")
})

test_that("sterling_ratio",{
  expect_equal(sterling_ratio(ww,t=12),1.082881,tolerance = 1e-6)
  expect_type(sterling_ratio(ww,t=12),"double")
})

