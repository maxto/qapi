# qapi

## Overview

Qapi provides a collection of functions for quantitative analysis in finance. The package aims to speed up calculations for large data manipulation, web app or micro-service. Code is minimal with a limited packages dependency,basic input requirements and errors handling, no missing values management.

The package can be easily applied to `matrix`, `data.frame`, `data.table`, `list`, etc. with `apply` family functions, `tidyverse` or custom classes.


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
- **avg_drawdown** - Average Drawdown
- **drawdown** - Drawdown
- **drawdown_info** - Drawdown information
- **excess_return** - Excess return
- **hist_cvar** - Historical Conditional Value At Risk
- **hist_var** - Historical Value At Risk
- **param_cvar** - Parametric Conditional Value At Risk
- **param_var** - Parametric Value At Risk
- **sharpe_ratio** - Sharpe ratio

### Stats

- **kurtosis** - Kurtosis
- **skewness** - Skewness

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

