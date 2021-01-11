#' Title
#'
#' @param tsio
#' @param obspyr
#' @param h
#' @param seas
#'
#' @return
#' @export
#'
#' @examples
getSegments <- function(tsio, obspyr, h = 0.15, seas = T){
  # Create time series object, needed as input for BFAST
  tsi <- ts(tsio, frequency = obspyr)
  # Convert the time series object into a dataframe, needed for the breakpoints function
  if(obspyr>1){
    datapp <- bfastpp(tsi, order = 1, lag = NULL, slag = NULL,
                      na.action = na.omit, stl = 'none')
  }else if(!seas){
    datapp <- data.frame(response = tsio, trend = seq(1:length(tsio)))
  }else{stop('No seasonal term allowed for time series with one observation per year or less.')}

  nreg <- switch(seas+1, 2, 5)
  # Test if enough observations are available to fit piecewise model
  if(floor(length(tsio[is.na(tsio)==F]) * h) > nreg){

    # Apply BFAST0n on time series: find breaks in the regression
    if (seas){
      bp <- breakpoints(response ~ trend + harmon, data = datapp, h = h)#, breaks = breaks
    } else{
      bp <- breakpoints(response ~ trend, data = datapp, h = h)##, breaks = breaks
    }
    # Check if BFAST0n found breakpoints
    # Extract BFAST trend component and breaks
    cf <- coef(bp)

    # Extract BFAST breaks
    if(!is.na(bp$breakpoints[1])){# at least one breakpoint found
      tbp <- bp$breakpoints #observation number of break (strucchange output)
      indna <- which(is.na(tsi)==F)
      tbp <- indna[tbp]   # correct observation number for missing values
      totbp <- tbp# observation number of breakpoints corrected for missing values
      bpf <- c(0, tbp, length(tsi))#corrected observation number of breakpoints, including first and last observation of the time series
    }else{
      tbp <- bp$breakpoints #observation number of break (strucchange output)
      totbp <- NA# observation number of breakpoints corrected for missing values
      bpf <- c(0, length(tsi))#corrected observation number of breakpoints, including first and last observation of the time series
    }

    #Derive trend component without missing values
    trf <- rep(NA,length(tsi))
    for(ti in 1:(length(bpf)-1)){
      trf[(bpf[ti]+1):bpf[ti+1]] <- cf[ti,1] + ((cf[ti,2]*((bpf[ti]+1):bpf[ti+1])))
    }

    # Get information criteria
    bp_loglik <- logLik(bp)
    bp_aic <- AIC(bp)[length(tbp[!is.na(tbp)]) + 1]

    out <- list(totbp, trf, bp_loglik, bp_aic)
    names(out) <- c('breakpoints', 'trend', 'loglik', 'AIC')

  }else{
    out <- list(NA, NA, NA, NA)
    names(out) <- c('breakpoints', 'trend', 'loglik', 'AIC')
  }

  out
}
