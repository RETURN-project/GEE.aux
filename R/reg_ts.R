#' Generate regular time series
#'
#' @param tsi vector of observations
#' @param dts vector of dates
#' @param fun function to aggregate ('max' or 'mean')
#' @param resol temporal resolution ('monthly', 'daily', or 'quart')
#'
#' @return regular time series
#' @export
#' @import bfast
#' @import zoo
#' @import stats
#' @import lubridate
#'
toRegularTS <- function(tsi, dts, fun, resol){
  tsi <- as.numeric(tsi)
  # monthly output
  dt <- 12 * (zoo::as.yearmon(max(dts)) - zoo::as.yearmon(min(dts)))
  if(resol == 'monthly'){
    # check if time span of observations is sufficient
    if(dt<1){stop('The time span of the time series is less than one month')}
    # make sure that there are at least two observations in subsequent months
    d <- ymd(min(dts))
    dts <- c(dts,as.Date(d %m+% months(1)))
    tsi <- c(tsi,NA)
    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearmon, mmax)) ## max
    } else if(fun == 'mean'){
      mz <- as.ts(aggregate(z, as.yearmon, function(x) mean(x,na.rm = TRUE))) ## mean
    }else if(fun == 'median'){
      mz <- as.ts(aggregate(z, as.yearmon, function(x) median(x,na.rm = TRUE))) ## mean
    }
    # daily output
  }else if (resol == 'daily'){
    mz <- bfastts(tsi, dts, type = "irregular")
    # quarterly output
  }else if (resol == 'quart'){
    # check if time span of observations is sufficient
    if(dt<3){stop('The time span of the time series is less than one quarter')}
    # make sure there are at least 2 observations in subsequent quarters
    d <- ymd(min(dts))
    dts <- c(dts,as.Date(d %m+% months(3)))
    tsi <- c(tsi,NA)

    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearqtr, mmax)) ## quarterly max
    }else if(fun == 'mean'){
      mz <- as.ts(aggregate(z, as.yearqtr, function(x) mean(x,na.rm = TRUE))) ## quarterly mean
    }else if(fun == 'median'){
      mz <- as.ts(aggregate(z, as.yearqtr, function(x) mean(x,na.rm = TRUE))) ## quarterly mean
    }
  }
  return(mz)
}


#' Helper function for the toRegularTS function
#'
#' @param x vector of observations
#'
#' @return the maximum value of the vector
#' @export
mmax <- function(x) {
  if(length(which.max(x)) == 0) {
    out <- NA
  } else {
    out <- as.numeric(x[which.max(x)])
  }
  return(out)
}

#' Get regular time series
#'
#' Convert irregular time series with missing values to a regular one
#'
#' @param dts vector of dates (formatted as strings YYYYMMDD)
#' @param obs vector of observations
#' @param fun function used to aggregate observations
#' @param resol desired resolution of the regular time series
#'
#' @return regular time series
#' @export
#'
getRegularTS <- function(dts,obs, fun = 'mean', resol ='monthly'){
  obs <- obs[!is.na(dts)]
  dts <- dts[!is.na(dts)]
  dts <- as.Date(as.character(dts),'%Y%m%d')
  regts <- toRegularTS(obs, dts, fun, resol)
  return(regts)
}
