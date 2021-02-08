# qapi

## Overview

Qapi provides a collection of low-level functions for quantitative analysis in finance. The package aims to optimize metrics for local data analysis, shiny web app or plumber-based microservice. Code is minimal with a limited packages dependency and basic input requirements. No missing values handling.

The package can be easily applied to `matrix`, `data.frame`, `data.table`, `list`, etc. with `apply` family functions or other packages, working on vectors and matrices.

Functions are implemented in pure R, for big data manipulation is advisable to use parallelization or C-based calculations.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("maxto/qapi")
```


## Functions

### Quants 

- **active_return** - Active return
- **adj_ann_sharpe_ratio** - Adjusted Annualized Sharpe ratio
- **ann_return** - Annualized return
- **ann_risk** - Annualized Risk
- **ann_sharpe_ratio** - Annualized Sharpe ratio
- **ann_sortino_ratio** - Annualized Sortino ratio
- **avg_drawdown** - Average Drawdown
- **calmar_ratio** - Calmar ratio
- **downside_potential** -  Downside Potential
- **downside_risk** - Downside risk
- **drawdown** - Drawdown
- **drawdown_info** - Drawdown information
- **excess_return** - Excess return
- **hist_cvar** - Historical Conditional Value At Risk
- **hist_var** - Historical Value At Risk
- **max_drawdown** - Max drawdown
- **omega_ratio** - Omega ratio
- **param_cvar** - Parametric Conditional Value At Risk
- **param_var** - Parametric Value At Risk
- **sharpe_ratio** - Sharpe ratio
- **sortino_ratio** - Sortino ratio
- **ulcer_index** - Ulcer index
- **upside_potential** - Upside potential
- **upside_risk** - Upside risk

### Stats

- **kurtosis** - Kurtosis
- **skewness** - Skewness
- **normpdf** - Normal probability density function (pdf)
- **norminv** - Inverse of the normal cumulative distribution function (cdf)

### Time series

- **rand_ts** - Random time series

## Usage

```r
library(qapi)

# rand time series
ts_mat <- rand_ts(m = 20,n = 3,as_ret = T,method="matrix")
ts_xts <- rand_ts(m = 20,n = 3,as_ret = T,method="xts")
ts_df <- rand_ts(m = 20,n = 3,as_ret = T,method="data.frame")

# single column or vector
sharpe_ratio(ts_mat[,1])
sharpe_ratio(ts_xts[,1])
sharpe_ratio(ts_df$s1)

# for multiple columns use an apply function
apply(ts_mat,2,function(a)sharpe_ratio(a))
apply(ts_xts,2,function(a)sharpe_ratio(a))
apply(ts_df,2,function(a)sharpe_ratio(a))

```

