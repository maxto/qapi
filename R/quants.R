
#' Annualized return
#'
#' Average annualized returns
#'
#' @param x asset returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_return(xx,t=12)
#'
ann_return <- function(x,t = 252,is_geom = TRUE) {
  n <- length(x)
  if (!is_geom) {
    return(mean(x) * t)
  }
  prod(1+x)^(t/n) - 1
}

#' Annualized Risk
#'
#' Annualized standard deviation
#'
#' @param x asset returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_risk(xx,t=12)
#'
ann_risk <- function(x,t=252) {
  stats::sd(x) * sqrt(t)
}



#' Active return
#'
#' Asset annualized return minus Benchmark annualized return
#'
#' @param x asset returns
#' @param y benchmark returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' yy <- c(0.04,-0.022,0.043,0.028,-0.078,-0.011,0.033,-0.049,0.09,0.087)
#' active_return(xx,yy,t=12)
#'
active_return <- function(x,y,t=252,is_geom=T) {
  ann_return(x,t,is_geom) - ann_return(y,t,is_geom)
}


#' Sharpe ratio
#'
#' Sharpe ratio for a vector of asset/portolio returns
#'
#' @param x asset returns
#' @param frisk annual free-risk rate. For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' sharpe_ratio(xx,frisk=0.02/12)
#'
sharpe_ratio <- function(x,frisk=0) {
  (mean(x) - frisk)/stats::sd(x)
}


#' Excess return
#'
#' Return on asset - risk free rate
#'
#' @param x asset returns
#' @param frisk annual free-risk rate. For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' excess_return(xx,frisk=0.02/12)
#'
excess_return <- function(x,frisk=0) {
  x - rep(frisk,1,length(x))
}


#' Annualized Sharpe ratio
#'
#' Sharpe ratio calculated on annualized excess return / annualized standard deviation
#'
#' @param x asset returns
#' @param frisk annual free-risk rate. For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' ann_sharpe_ratio(xx,frisk=0.02/12,t=12)
#'
ann_sharpe_ratio <- function(x,frisk=0,t=252,is_geom=TRUE) {
  xs <- excess_return(x,frisk)
  ann_return(xs,t,is_geom)/ann_risk(x,t)
}


#' Adjusted Annualized Sharpe ratio
#'
#' Sharpe Ratio adjusted for skewness and kurtosis with a penalty factor for negative skewness and excess kurtosis
#'
#' @param x asset returns
#' @param frisk annual free-risk rate. For daily rate (frisk/252), weekly (frisk/52), monthly (frisk/12), etc
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,0.014,0.024,0.015,0.066,-0.014,0.039)
#' adj_ann_sharpe_ratio(xx,frisk=0.02/12,t=12)
#'
adj_ann_sharpe_ratio <- function(x,frisk=0,t=252,is_geom=TRUE) {
  sr <- ann_sharpe_ratio(x,frisk,t,is_geom)
  sk <- skewness(x)
  ku <- kurtosis(x)
  sr * (1 + (sk/6) * sr - ((ku - 3)/24) * (sr)^2)
}

#' Drawdown
#'
#' Calculate drawdown array from asset returns. Return a vector with numbers >= 0
#'
#' @param x asset returs
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric vector of length x
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' drawdown(xx)
#'
drawdown <- function(x,is_geom=TRUE) {
  v <- cumprod(1 + x)
  if (!is_geom) {
    v <- 1 + cumsum(x)
  }
  1 - v/cummax(v)
}

#' Drawdown information
#'
#' Return a list of drawdown info: value, start index, trough index, end index, duration, peak-to-trough, recovery period
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return list
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' drawdown_info(xx)
#'
drawdown_info <- function(x,is_geom=TRUE) {
  if (class(x) != "numeric") {
    x <- as.numeric(x) #force to be numeric vector
  }
  dd <- drawdown(x,is_geom)
  #init
  dd_ <- c()
  dds <- sign(dd)
  last_dd <- dd[1]
  last_pos <- dds[1]
  idx <- 1
  sd <- 1
  ed <- 1
  last_dd_idx <- 1
  dd_start <- c()
  dd_trough <- c()
  dd_end <- c()
  for (i in 1:length(dd)) {
    if (dds[i] == last_pos) {
      if (dd[i] > last_dd) {
        last_dd <- dd[i]
        last_dd_idx <- i
      }
      ed <- i + 1
    } else {
      if (last_pos == 1) {
        dd_[idx] <- last_dd
        dd_start[idx] <- sd
        dd_trough[idx] <- last_dd_idx
        dd_end[idx] <- ed
        idx <- idx + 1
      }
      sd <- i
      last_dd <- dd[i]
      last_dd_idx <- i
      ed <- i + 1
      last_pos <- dds[i]
    }
  }
  if (last_pos == 1) {
    dd_[idx] <- last_dd
    dd_start[idx] <- sd
    dd_trough[idx] <- last_dd_idx
    dd_end[idx] <- ed
  }
  return (list("dd_val"=dd_,"dd_start"=dd_start,"dd_trough"=dd_trough,"dd_end"= dd_end,
               "dd_length" = (dd_end - dd_start + 1),"dd_peak_trough"= (dd_trough - dd_start + 1),
               "dd_recov" = dd_end - dd_trough))

}

#' Continous drawdawn
#'
#' Return largest individual drawdowns
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' cdrawdown(xx)
#'
cdrawdown <- function(x,is_geom=TRUE) {
  cdd <- c()
  tt <- 1
  tmp <- 0
  for (i in 1:length(x)) {
    if (x[i] < 0) {
      if (tmp == 0) {
        tmp <- 1 + x[i]
      } else {
        if (is_geom) {
          tmp <- tmp * (1 + x[i])
        } else {
          tmp <- tmp + x[i]
        }
      }
    }
    if (x[i] >= 0) {
      if (tmp != 0) {
        cdd[tt] = 1 - tmp
        tt <- tt+1
        tmp <- 0
      }
    }
  }
  if (tmp != 0) {
    cdd <- c(cdd,1-tmp)
    tmp <- 0
  }
  cdd
}



#' Average Drawdown
#'
#' Return the average drawdown from asset returns
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' avg_drawdown(xx)
#'
avg_drawdown <- function(x,is_geom=TRUE) {
  mean(drawdown_info(x,is_geom)[["dd_val"]])
}

#' Maximum Drawdown
#'
#' Return the maximum drawdown from asset returns
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' max_drawdown(xx)
#'
max_drawdown <- function(x,is_geom=TRUE) {
  max(drawdown(x,is_geom))
}

#' Ulcer index
#'
#' RUlcer Index of Peter G. Martin (1987). The impact of long, deep drawdowns will have significant impact because the underperformance since the last peak is squared
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' ulcer_index(xx)
#'
ulcer_index <- function(x,is_geom=TRUE) {
  sqrt(sum(drawdown(x,is_geom)^2)/length(x))
}

#' Calmar ratio
#'
#' A risk-adjusted measure like Sharpe ratio that uses maximum drawdown instead of standard deviation for risk
#'
#' @param x asset returns
#' @param ann_frisk annual free risk value
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' calmar_ratio(xx,ann_frisk = 0.01,t=12)
#'
calmar_ratio <- function(x,ann_frisk=0,t=252,is_geom=TRUE) {
  (ann_return(x,t,is_geom) - ann_frisk)/max_drawdown(x,is_geom)
}


#' Historical Value At Risk
#'
#' Historical value at risk
#'
#' @param x asset returns
#' @param p confidence level
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' hist_var(xx,p=0.95)
#'
hist_var <- function(x,p=0.95) {
  stats::quantile(x,1-p,type = 7,names = F) #type 7
}

#' Parametric Value At Risk
#'
#' Parametric or gaussian value at risk
#'
#' @param x asset returns
#' @param p confidence level
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' param_var(xx,p=0.95)
#'
param_var <- function(x,p=0.95) {
  stats::sd(x)*norminv(1-p)+mean(x)
}


#' Historical Conditional Value At Risk
#'
#' Historical conditional value at risk
#'
#' @param x asset returns
#' @param p confidence level
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' hist_cvar(xx,p=0.95)
#'
hist_cvar <- function(x,p=0.95) {
  mean(x[x<=hist_var(x,p)])
}

#' Parametric Conditional Value At Risk
#'
#' Parametric Conditional Value-At-Risk. More sensitive to the shape of the loss distribution in the tails.
#' Also known as Expected Shortfall (ES), Expected Tail Loss (ETL).
#'
#' @param mu mean value
#' @param sigma standard deviation
#' @param p confidence level
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' param_cvar(mean(xx),sd(xx))
#'
param_cvar <- function(mu=0,sigma=1,p=0.95) {
  -sigma*normpdf(norminv(1-p))/(1-p)+mu
}


#' Downside risk
#'
#' Downside Risk or Semi-Standard Deviation.Measures  the  variability  of  underperformance  below  a  minimum  target   rate
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' downside_risk(xx,mar=0.1/100)
#'
downside_risk <- function(x,mar=0) {
  sqrt(sum(ifelse(x-mar>0,0,x-mar)^2)/length(x))
}

#' Downside potential
#'
#' Downside potential is the first lower partial moment
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' downside_potential(xx)
#'
downside_potential <- function(x,mar=0) {
  -sum(ifelse(x-mar>0,0,x-mar))/length(x)
}

#' Sortino ratio
#'
#' Sortino ratio
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' sortino_ratio(xx)
#'
sortino_ratio <- function(x,mar=0) {
  mean(excess_return(x,mar))/downside_risk(x,mar)
}

#' Annualized Sortino ratio
#'
#' Annualized sortino ratio
#'
#' @param x asset returns
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param ann_mar annual minimum acceptable return
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' ann_sortino_ratio(xx,t=12,ann_mar=0.01)
#'
ann_sortino_ratio <- function(x,t=252,ann_mar=0,is_geom=TRUE) {
  (ann_return(x,t,is_geom) - ann_mar)/(downside_risk(x,mar=ann_mar/t)*sqrt(t))
}

#' Upside risk
#'
#' Measures  the  variability  of  overperformance  above  a  minimum  target   rate
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' upside_risk(xx,mar=0.1/100)
#'
upside_risk <- function(x,mar=0) {
  sqrt(sum(ifelse(x-mar<0,0,x-mar)^2)/length(x))
}

#' Upside potential
#'
#' Upside potential
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' upside_potential(xx,mar=0.1/100)
#'
upside_potential <- function(x,mar=0) {
  sum(ifelse(x-mar<0,0,x-mar)/length(x))
}

#' Omega ratio
#'
#' A gain/loss ratio: upside potential/downside potential
#'
#' @param x asset returns
#' @param mar minimum acceptable return
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' omega_ratio(xx)
#'
omega_ratio <- function(x,mar=0) {
  upside_potential(x,mar)/downside_potential(x,mar)
}


#' Martin ratio
#'
#' A risk-adjusted measure with free risk and Ulcer index.
#'
#' @param x asset returns
#' @param ann_frisk annual free risk
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' martin_ratio(xx,ann_frisk=0.01,t=12)
#'
martin_ratio <- function(x,ann_frisk=0,t=252,is_geom=TRUE) {
  (ann_return(x,t,is_geom) - ann_frisk)/ulcer_index(x,is_geom)
}

#' Pain index
#'
#' Mean value of the drawdowns, similar to Ulcer Index
#'
#' @param x asset returns
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' pain_index(xx)
#'
pain_index <- function(x,is_geom=TRUE) {
  sum(drawdown(x,is_geom))/length(x)
}

#' Pain ratio
#'
#' A risk-adjusted measure with free risk and Pain index
#'
#' @param x asset returns
#' @param ann_frisk annual free risk
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' pain_ratio(xx,ann_frisk=0.01,t=12)
#'
pain_ratio <- function(x,ann_frisk=0,t=252,is_geom=TRUE) {
  (ann_return(x,t,is_geom) - ann_frisk)/pain_index(x,is_geom)
}

#' Burke ratio
#'
#' A risk-adjusted measure with free risk and drawdowns. For the 'simple' mode the excess return over free risk is divided by the square root of
#' the sum of the square of the drawdowns. For the 'modified' mode the Burke Ratio is multiplied
#' by the square root of the number of data.
#'
#' @param x asset returns
#' @param ann_frisk annual free risk
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#' @param method "simple" or "modified" (def: "simple")
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' burke_ratio(xx,ann_frisk=0.01,t=12)
#' burke_ratio(xx,ann_frisk=0.01,t=12,method="modified")
#'
burke_ratio <- function(x,ann_frisk=0,t=252,is_geom=TRUE,method=c("simple","modified")) {
  aret <- ann_return(x,t,is_geom)
  cdd <- sqrt(sum(cdrawdown(x)^2))
  method <- method[1]
  switch(method,
         "simple" = return ((aret - ann_frisk)/cdd),
         "modified" = return ((aret - ann_frisk)/(cdd*sqrt(length(x)))),
         stop("unknown method")
  )
}


#' Sterling ratio
#'
#' A risk-adjusted measure like Calmar ratio but the denominator is the largest consecutive drawdown (excluded the 10% excess in the original formula)
#'
#' @param x asset returns
#' @param ann_frisk annual free risk
#' @param t frequency of data. 1: yearly, 4: quarterly, 12: monthly, 52: weekly, 252: daily
#' @param is_geom TRUE geometric, FALSE simple
#'
#' @return numeric
#' @export
#'
#' @examples
#' xx <- c(0.003,0.026,0.015,-0.009,-0.014,-0.024,0.015,0.066,-0.014,0.039)
#' sterling_ratio(xx,ann_frisk=0.01,t=12)
#'
sterling_ratio <- function(x,ann_frisk=0,t=252,is_geom=TRUE) {
  (ann_return(x,t,is_geom) - ann_frisk)/max(cdrawdown(x,is_geom))
}
