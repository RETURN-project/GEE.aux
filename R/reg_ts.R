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
#'
toRegularTS <- function(tsi, dts, fun, resol){
  tsi <- as.numeric(tsi)
  # monthly output
  if(resol == 'monthly'){
    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearmon, mmax)) ## max
    } else if(fun == 'mean'){
      mz <- as.ts(aggregate(z, as.yearmon, function(x) mean(x,na.rm = TRUE))) ## mean
    }
    # daily output
  }else if (resol == 'daily'){
    mz <- bfastts(tsi, dts, type = "irregular")
    # quarterly output
  }else if (resol == 'quart'){
    z <- zoo(tsi, dts) ## create a zoo (time) series
    if(fun == 'max'){
      mz <- as.ts(aggregate(z, as.yearqtr, mmax)) ## quarterly max
    }
    if(fun == 'mean'){
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
#'
#' @return regular time series
#' @export
#'
getRegularTS <- function(dts,obs){
  obs <- obs[!is.na(dts)]
  dts <- dts[!is.na(dts)]
  dts <- as.Date(as.character(dts),'%Y%m%d')
  regts <- toRegularTS(obs, dts, 'mean', 'monthly')
  return(regts)
}
