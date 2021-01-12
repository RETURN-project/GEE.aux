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


#' Title
#'
#' @param dts
#' @param obs
#'
#' @return
#' @export
#'
getRegularTS <- function(dts,obs){
  obs <- obs[!is.na(dts)]
  dts <- dts[!is.na(dts)]
  dts <- as.Date(as.character(dts),'%Y%m%d')
  regts <- toRegularTS(obs, dts, 'mean', 'monthly')
  return(regts)
}
