
#' YrYr recovery function
#'
#' Year on year average
#'
#' @param ts Vector containing the times (same size as ys)
#' @param ys Vector containing the values (same size as ts)
#' @param tpert Time of the perturbation. Default set to 0 yr
#' @param deltat Reference recovery time (in years). Default set to 5 yr
#'
#' @return The YrYr parameter for the given time series
#' @export
#' @import stats
#'
#' @references Frazier, R. J., Coops, N. C., Wulder, M. A., Hermosilla, T., & White, J. C. (2018).
#' Analyzing spatial and temporal variability in short-term rates of post-fire vegetation return
#' from Landsat time series. Remote Sensing of Environment, 205, 32–45.
#' \url{https://doi.org/10.1016/j.rse.2017.11.007}
#'
#' @examples
#' # Generate an example time series
#' ts <- seq(-2, 10, by = 0.1) # as a vector of times
#' ys <- 3 + 2 * ts # plus a vector of values
#' yryr(ts, ys)
yryr <- function(ts, ys, tpert=0, deltat=5) {
  # Check input
  if ((min(tpert) < min(ts, na.rm = TRUE)) || (min(tpert) + max(deltat) > max(ts, na.rm = TRUE))) {
    # if ((tpert < min(ts, na.rm = TRUE)) || (tpert + deltat > max(ts, na.rm = TRUE))) {
    stop("Error: 'tpert' and/or 'tpert + deltat' are outside the bounds imposed by 'ts'")
  }

  # Auxiliary interpolation function. Given a time, returns the corresponding value.
  # If the time is in ts, returns the corresponding ys. If the time is not in ts,
  # returns a linearly interpolated value
  V <- approxfun(x = ts, y = ys)

  dy <- (mean(V(min(tpert, na.rm = T) + deltat)) - mean(V(tpert)))
  dx <- (mean(min(tpert, na.rm = T) + deltat) - mean(tpert))

  # The result is the mean slope between t = tpert and t = tpert + deltat
  return(dy / dx)
}

#' R80p recovery function
#'
#' Ratio of Eighty Percent (R80P). The ratios can be customized.
#'
#' @param ts Vector containing the times (same size as ys). Perturbation assumed to happen at 0
#' @param ys Vector containing the values (same size as ts)
#' @param r Ratio. Default set to 0.8
#' @param ts_pre Sampling times for estimating Vpre. Default set to -1
#' @param ts_post Sampling times for estimating Vpost. Default set to c(4, 5)
#'
#' @return The R80p indicator (R_r_p if r != 0.8 is provided)
#' @export
#'
#' @references Frazier, R. J., Coops, N. C., Wulder, M. A., Hermosilla, T., & White, J. C. (2018).
#' Analyzing spatial and temporal variability in short-term rates of post-fire vegetation return
#' from Landsat time series. Remote Sensing of Environment, 205, 32–45.
#' \url{https://doi.org/10.1016/j.rse.2017.11.007}
#'
#' @examples
#' # Generate an example time series
#' ts <- seq(-2, 10, by = 0.1) # as a vector of times
#' ys <- 1*(ts<0)+ (-1 + (2/0.5)*(ts))*(ts >= 0)*(ts <= 0+0.5)+(ts > 0.5) # plus a vector of values
#' r80p(ts, ys)
r80p <- function(ts, ys, r=0.8, ts_pre=c(-1,-2), ts_post=c(4, 5)) {
  # Auxiliary interpolation function. Given a time, returns the corresponding value.
  # If the time is in ts, returns the corresponding ys. If the time is not in ts,
  # returns a linearly interpolated value
  V <- approxfun(x = ts, y = ys)

  # The typical value before perturbation is defined as the average of the values
  # sampled at the times contained in ts_pre
  Vpre  <- mean(V(ts_pre))

  # The typical value after perturbation is defined as the maximum of the values
  # sampled at the times contained in ts_post
  Vpost <- max(V(ts_post))

  # Return the result
  return(Vpost / (Vpre * r))
}

#' RRI recovery function
#'
#' Relative Recovery Indicator
#'
#' @param ts Vector containing the times (same size as ys). Perturbation assumed to happen at 0
#' @param ys Vector containing the values (same size as ts)
#' @param ts_pre Sampling times for estimating Vpre. Default set to -1
#' @param ts_post Sampling times for estimating Vpost. Default set to c(4, 5)
#' @param tpert Time of the perturbation. Default set to 0 yr
#'
#' @return The RRI indicator
#' @export
#'
#' @references Frazier, R. J., Coops, N. C., Wulder, M. A., Hermosilla, T., & White, J. C. (2018).
#' Analyzing spatial and temporal variability in short-term rates of post-fire vegetation return
#' from Landsat time series. Remote Sensing of Environment, 205, 32–45.
#' \url{https://doi.org/10.1016/j.rse.2017.11.007}
#'
#' @examples
#' # Generate an example time series
#' ts <- seq(-2, 10, by = 0.1) # as a vector of times
#' ys <- 1*(ts<0)+ (-1 + (2/0.5)*(ts))*(ts >= 0)*(ts <= 0+0.5)+(ts > 0.5)#texponential(ts, pert = -2, offset = 1, thalf = 0.25) # plus a vector of values
#' rri(ts, ys)
rri <- function(ts, ys, tpert=0, ts_pre=-1, ts_post=c(4, 5)) {
  # Auxiliary interpolation function. Given a time, returns the corresponding value.
  # If the time is in ts, returns the corresponding ys. If the time is not in ts,
  # returns a linearly interpolated value
  V <- approxfun(x = ts, y = ys)

  # The disturbance is assumed to happen at t = 0
  Vdist <- mean(V(tpert))

  # The typical value before perturbation is defined as the average of the values
  # sampled at the times contained in ts_pre
  Vpre  <- mean(V(ts_pre))

  # The typical value after perturbation is defined as the maximum of the values
  # sampled at the times contained in ts_post
  Vpost <- max(V(ts_post))

  # The deltas are closely related to the typical values
  delta_dist <- abs(Vdist - Vpre)
  delta_rec <- abs(Vpost - Vdist)

  return(delta_rec / delta_dist)
}

#' Calculate recovery metrics
#'
#' Calculate recovery metrics from a time series with known disturbance date. The calcFrazier function derives the RRI, R80P and YrYr recovery indicators,
#' defined by Frazier et al. (2018). The indicators are originally developed for annual long-term time series of optical vegetation indices.
#' Yet, in order to be able to derive the indicators as well for dense and/or short time series, a modified version is suggested.
#' Here, the user can define the time period before, during and after the disturbance that is used to derive the indicators.
#' To reduce the interference of the seasonal pattern of dense time series, the chosen time period should cover blocks of n years.
#' (Frazier, R. J., Coops, N. C., Wulder, M. A., Hermosilla, T., & White, J. C. (2018). Analyzing spatial and temporal variability in short-term rates
#' of post-fire vegetation return from Landsat time series. Remote Sensing of Environment, 205, 32-45.)
#'
#' @param tsio vector of observations (time series with a fixed observation frequency)
#' @param tdist observation number of disturbance, indicating the timing of the disturbance
#' @param obspyr number of observations per year
#' @param nPre number of years prior to the disturbance used to calculate the pre-disturbance value
#' @param nPost number of years used to calculate the post-disturbance value
#' @param nPostStart indicates the start of the post-disturbance period (expressed as number of years after the disturbance)
#' @param nDelta number of years used to calculate the post-disturbance value for the YrYr metric
#' @param nDeltaStart indicates the start of the post-disturbance period for the YrYr metric (expressed as number of years after the disturbance)
#' @param nDist number of years used to quantify the state during the disturbance
#'
#' @return a list containing the RRI recovery indicator, R80p recovery indicator and YrYr recovery indicator
#' @export
#'
calcFrazier <- function(tsio, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart){
  # check if there are enough observations before and after the disturbance to calculate the metrics
  if ( (tdist > (nPre*obspyr) ) & ( (tdist + ((nPost+nPostStart)*obspyr) - 2) < length(tsio)  ) & ( sum(!is.na(tsio)) > 2) ) {
    # translate parameters to those needed for the recovery functions
    ys <- tsio# response values of time series
    ts <- seq(1, length(tsio))# observation numbers of time series
    # the observations during the perturbation
    if (nDist == 0 ){# if annual observatons or if duration of perturbation equals one time step
      tpert <- tdist
    }else if (obspyr == 1){
      tpert <- seq(tdist, tdist + nDist-1 )
    }else{
      tpert <- seq(tdist, tdist + nDist*obspyr - 1)
    }
    # the observations that represent the pre-disturbed state
    ts_pre <- seq(tdist - nPre*obspyr, tdist - 1)

    # the observations that represent the post-disturbed state
    ts_post <-  seq(tdist + (nPostStart*obspyr), tdist + ((nPostStart + nPost)*obspyr) - 1)

    # the observations that define the post-disturbance state for YrYr metric (relative to start disturbance)
    # the post-disturbance period for the YrYr metric is defined as the last year of the regular post-disturbance period
    deltat <- seq(nDeltaStart*obspyr,(nDelta+nDeltaStart)*obspyr-1)#ts_post[(length(ts_post) - obspyr+1):length(ts_post)]-tdist

    # Derive recovery indicators
    RRI <- rri(ts, ys, tpert, ts_pre, ts_post)
    R80P <- r80p(ts, ys, r = 0.8, ts_pre, ts_post)
    YrYr <- yryr(ts, ys, tpert, deltat)
    # make list of recovery indicators as output of the function
    lst <- list(RRI, R80P, YrYr)
    names(lst) <- c('RRI', 'R80P', 'YrYr')
    # give NA as output if not able to calculate the recovery indicators
  } else {
    lst <- list(NA, NA, NA)
    names(lst) <- c('RRI', 'R80P', 'YrYr')
  }
  lst
}

#' Calculate recovery metrics
#'
#' @param tsi vector of observations (time series with a fixed observation frequency)
#' @param tdist observation number of disturbance, indicating the timing of the disturbance
#' @param obspyr number of observations per year
#' @param nPre number of years prior to the disturbance used to calculate the pre-disturbance value
#' @param nDist number of years used to quantify the state during the disturbance
#' @param nPost number of years used to calculate the post-disturbance value
#' @param nPostStart indicates the start of the post-disturbance period (expressed as number of years after the disturbance)
#' @param nDelta number of years used to calculate the post-disturbance value for the YrYr metric
#' @param nDeltaStart indicates the start of the post-disturbance period for the YrYr metric (expressed as number of years after the disturbance)
#' @param tbp breakpoints in time series
#' @param selBreak How should the breakpoint be selected to calculate recovery metrics: max = maximum break, close =  the break closed to tdist, first = first negative break (if no negative breaks are found, it takes the first break).
#' @param chkBrk should time series be checked for breaks within the post- or pre-disturbance period?
#' @param timeThres the maximum allowed time difference between the observed break and the disturbance [years]
#'
#' @return
#' @export
#'
calcRecMetrics <- function(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                       tbp, selBreak, chkBrk = F, timeThres){
  if(!is.na(tbp[1])){
    totbp <- tbp
    if(selBreak == 'max'){
      # Find the major break
      dbr <- tsi[tbp+1]-tsi[tbp]
      tbp <- tbp[which(abs(dbr) == max(abs(dbr)))]
      tbp <- tbp[1]
    }else if(selBreak == 'close'){
      # Use the break closest to the disturbance date
      dbr <- tbp-tdist
      tbp <- tbp[which(abs(dbr) == min(abs(dbr)))]
      tbp <- tbp[1]
      # Use the first negative break. If no negative breaks are present, use first break
    }else if(selBreak == 'first'){
      dbr <- tsi[tbp+1]-tsi[tbp]
      tbp <- tbp[which(dbr<0)]
      if(length(tbp)>0){tbp <- tbp[1]}else{tbp <- totbp[1]}
    }

    # check the time period between the break and the disturbance
    deltat <- tbp-tdist
    timeChck <- ((abs(deltat)/obspyr) < timeThres)

    # check the typology of the segments:
    # positive post-disturbance slope
    postChck <- ((tsi[tbp+3] - tsi[tbp+2]) > 0)
    # negative break
    distChck <- ((tsi[tbp+1] - tsi[tbp]) < 0)
    # no negative break in recovery period
    brkthres <- (nPostStart*obspyr) + (nPost*obspyr) #post-disturbance period used to assess recovery
    if(any((totbp>tbp) & (totbp<(brkthres+tbp)))){
      postbr <- totbp[(totbp>tbp) & (totbp<(brkthres+tbp))]
      postdbr <- tsi[postbr+1]-tsi[postbr]
      brkChck <- !any((postdbr< -0.1))
    }else{
      brkChck <- TRUE
    }
    if(!chkBrk){brkChck = T}

    # no negative break in pre-disturbance period
    brkthres <- (nPre*obspyr) #post-disturbance period used to assess recovery
    if(any((totbp<tbp) & (totbp>(tbp-brkthres)))){
      prebr <- totbp[(totbp<tbp) & (totbp>(tbp-brkthres))]
      predbr <- tsi[prebr+1]-tsi[prebr]
      brkChckPre <- !any((predbr< -0.1))
    }else{
      brkChckPre <- TRUE
    }
    if(!chkBrk){brkChckPre = T}

    if(timeChck & postChck & distChck & brkChck & brkChckPre){
      # Calculate Frazier recovery metrics on BFAST trend component
      frz <- calcFrazier(as.numeric(tsi), (tbp+1), obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart)
    }else{
      frz <- list(NA, NA, NA)
      names(frz) <- c('RRI', 'R80P', 'YrYr')
    }
  }else{
    frz <- list(NA, NA, NA)
    names(frz) <- c('RRI', 'R80P', 'YrYr')
  }
frz
}

