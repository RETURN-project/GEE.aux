
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

# calculate recovery metrics
calcRecMetrics <- function(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                       tbp, maxBreak, chkBrk = F, timeThres){
  if(!is.na(tbp[1])){
    totbp <- tbp
    if(maxBreak){
      # Find the major break
      dbr <- trf[tbp+1]-trf[tbp]
      tbp <- tbp[which(abs(dbr) == max(abs(dbr)))]
    }else{
      # Use the break closest to the disturbance date
      dbr <- tbp-tdist
      tbp <- tbp[which(abs(dbr) == min(abs(dbr)))]
    }

    # check the time period between the break and the disturbance
    timeChck <- ((min(abs(dbr))/obspyr) < timeThres)

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

    if(timeChck & postChck & distChck & brkChck){
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

