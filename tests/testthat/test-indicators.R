context("Recovery indicator: YrYr")

test_that("Straight line time series", {
  # Generate a time series (straight line of slope m and intercept c)
  ts <- seq(-2, 10, by = 0.1)
  c <- 10
  m <- 3.14
  ys <- c + m * ts

  expect_equal(yryr(ts, ys), m, tolerance = 1e-6)
  expect_equal(yryr(ts, ys, tpert = -1, deltat = 3), m, tolerance = 1e-6)
})

test_that("Sinusoidal time series", {
  # Generate a time series (straight line of slope m and intercept c)
  ts <- seq(-2*pi, 10*pi, by = 0.01*pi)
  ys <- sin(ts)

  # Sampling after whole periods should yield a 0 mean slope
  expect_equal(yryr(ts, ys, tpert = 0, deltat = 1*pi), 0, tolerance = 1e-6)
  expect_equal(yryr(ts, ys, tpert = 0, deltat = 2*pi), 0, tolerance = 1e-6)
  expect_equal(yryr(ts, ys, tpert = pi, deltat = 2*pi), 0, tolerance = 1e-6)
})

test_that("NAs are tolerated", {
  # Generate a time series (straight line of slope m and intercept c)
  ts <- seq(-2*pi, 10*pi, by = 0.01*pi)
  ts[30] <- NA
  ys <- sin(ts)

  # Sampling after whole periods should yield a 0 mean slope
  expect_equal(yryr(ts, ys, tpert = 0, deltat = 1*pi), 0, tolerance = 1e-6)
  expect_equal(yryr(ts, ys, tpert = 0, deltat = 2*pi), 0, tolerance = 1e-6)
  expect_equal(yryr(ts, ys, tpert = pi, deltat = 2*pi), 0, tolerance = 1e-6)
})

test_that("Non matching lengths", {
  # Generate a bad time series (different vector sizes)
  ts <- seq(-2, 10, by = 0.1)
  ys <- seq(-2, 10, by = 0.2)

  expect_error(yryr(ts, ys),
               "'x' and 'y' lengths differ")
})

test_that("Out of bounds", {
  # Generate a bad time series (different vector sizes)
  ts <- seq(-2, 10, by = 0.1)
  ys <- seq(-2, 10, by = 0.1)

  expect_error(yryr(ts, ys, tpert = 9, deltat = 2),
               "*bounds*")
})

context("Recovery indicator: R80p")

test_that("Fully recovered time series", {
  # Generate a time series
  ts <- seq(-2, 10, by = 0.1)
  ys <- c(rep(1,20),seq(-1,1,by=0.2),rep(1,90))#exponential(ts, pert = -2, offset = 1, thalf = 0.5)

  expect_equal(r80p(ts, ys), 1 / 0.8, tolerance = 1e-2)
})

test_that("NAs are tolerated", {
  # Generate a time series
  ts <- seq(-2, 10, by = 0.1)
  ts[5] <- NA # Introduce NA
  ys <- c(rep(1,20),seq(-1,1,by=0.2),rep(1,90))#exponential(ts, pert = -2, offset = 1, thalf = 0.5)

  expect_equal(r80p(ts, ys), 1 / 0.8, tolerance = 1e-2)
})

context("Recovery indicator: RRI")

test_that("Fully recovered time series", {
  # Generate a time series
  ts <- seq(-2, 10, by = 0.1) # as a vector of times
  ys <- c(rep(1,20),seq(-1,1,by=0.4),rep(1,95))#exponential(ts, pert = -2, offset = 1, thalf = 0.25) # plus a vector of values

  expect_equal(rri(ts, ys), 1, tolerance = 1e-4)
})

test_that("NAs are tolerated", {
  # Generate a time series
  ts <- seq(-2, 10, by = 0.1)
  ts[5] <- NA # Introduce NA
  ys <- c(rep(1,20),seq(-1,1,by=0.4),rep(1,95))#exponential(ts, pert = -2, offset = 1, thalf = 0.25) # plus a vector of values

  expect_equal(rri(ts, ys), 1, tolerance = 1e-2)
})

context("Disturbance magnitude")

test_that("NAs are tolerated", {
  # Generate a time series
  ts <- seq(1, 121, by = 1)
  ts[5] <- NA # Introduce NA
  ys <- c(rep(1,20),seq(-1,1,by=0.4),rep(1,95))#exponential(ts, pert = -2, offset = 1, thalf = 0.25) # plus a vector of values

  expect_equal(d_dist(ts, ys, tpert=21, ts_pre=20), 2, tolerance = 1e-2)
})

context("pre-disturbance state")

test_that("NAs are tolerated", {
  # Generate a time series
  ts <- seq(1, 121, by = 1)
  ts[5] <- NA # Introduce NA
  ys <- c(rep(1,20),seq(-1,1,by=0.4),rep(1,95))#exponential(ts, pert = -2, offset = 1, thalf = 0.25) # plus a vector of values

  expect_equal(V_pre(ts, ys, ts_pre=20), 1, tolerance = 1e-2)
})

context("Calculate recovery metrics")

test_that("Frazier - annual - too short time series", {

  tsio <- c(rep(0,1), seq(-1, 0), rep(0,1))
  tdist <- 2
  obspyr <- 1
  shortDenseTS <- FALSE
  nPre <- 2
  nDist <- 1
  nPostStart <- 4
  nPost <- 2
  nDeltaStart <- 5
  nDelta <- 1

  metrics <- calcFrazier(tsio, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart)

  expect_equal(metrics$RRI, NA)
  expect_equal(metrics$R80P, NA)
  expect_equal(metrics$YrYr, NA)
})

test_that("Frazier - annual", {

  tsio <- c(rep(1,2), seq(-5, -1), rep(-2,1))
  tdist <- 3
  obspyr <- 1
  shortDenseTS <- FALSE
  nPre <- 2
  nDist <- 1
  nPostStart <- 4
  nPost <- 2
  nDeltaStart <- 5
  nDelta <- 1

  metrics <- calcFrazier(tsio, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart)
  pre <- 1
  dnbr <- 6
  ari <- 4

  rrim <- ari/dnbr
  r80pm <- -1/(0.8*pre)
  yryrm <- (-2 + 5)/5

  expect_equal(metrics$RRI, rrim, tolerance = 1e-4)
  expect_equal(metrics$R80P, r80pm, tolerance = 1e-4)
  expect_equal(metrics$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(metrics$impact, dnbr, tolerance = 1e-4)
  expect_equal(metrics$Vpre, pre, tolerance = 1e-4)
})

test_that("Frazier - dense", {

  tsio <- c(rep(1,24), seq(-5, -1, length.out=72), rep(-2,12))
  tdist <- 25
  obspyr <- 12
  shortDenseTS <- TRUE
  nPre <- 2
  nDist <- 1
  nPostStart <- 4
  nPost <- 2
  nDeltaStart <- 5
  nDelta <- 1

  metrics <- calcFrazier(tsio, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart)
  pre <- 1
  dist <- mean(tsio[25:36])
  post <- max(tsio[73:96])
  dnbr <- pre-dist
  ari <- post-dist

  rrim <- ari/dnbr
  r80pm <- post/(0.8*pre)
  yryrm <- (mean(tsio[85:96]) - dist)/(5*12)

  expect_equal(metrics$RRI, rrim, tolerance = 1e-4)
  expect_equal(metrics$R80P, r80pm, tolerance = 1e-4)
  expect_equal(metrics$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(metrics$impact, dnbr, tolerance = 1e-4)
  expect_equal(metrics$Vpre, pre, tolerance = 1e-4)
})


test_that("calcRecMetrics - timing first disturbance", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
   chkBrk <- T
  timeThres <- 20

  # first
  selBreak <- 'first'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                             tbp, selBreak, chkBrk = F, timeThres)

  pre <- 1
  dist <- mean(tsi[25:36])
  post <- max(tsi[73:96])
  dnbr <- pre-dist
  ari <- post-dist

  rrim <- ari/dnbr
  r80pm <- post/(0.8*pre)
  yryrm <- (mean(tsi[73:96]) - dist)/(mean(c(73:96)) - mean(c(25:36)))

  expect_equal(met$RRI, rrim, tolerance = 1e-4)
  expect_equal(met$R80P, r80pm, tolerance = 1e-4)
  expect_equal(met$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(met$impact, dnbr, tolerance = 1e-4)
  expect_equal(met$Vpre, pre, tolerance = 1e-4)
})

test_that("calcRecMetrics - timing close disturbance", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 20

  # close
  selBreak <- 'close'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  pre <- -1
  dist <- mean(tsi[121:132])
  post <- max(tsi[169:192])
  dnbr <- pre-dist
  ari <- post-dist

  rrim <- ari/dnbr
  r80pm <- post/(0.8*pre)
  yryrm <- (mean(tsi[169:192]) - dist)/(mean(c(169:192)) - mean(c(121:132)))

  expect_equal(met$RRI, rrim, tolerance = 1e-4)
  expect_equal(met$R80P, r80pm, tolerance = 1e-4)
  expect_equal(met$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(met$impact, dnbr, tolerance = 1e-4)
  expect_equal(met$Vpre, pre, tolerance = 1e-4)
})

test_that("calcRecMetrics - timing max disturbance", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 20

  # close
  selBreak <- 'max'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  pre <- 0.5
  dist <- mean(tsi[217:228])
  post <- max(tsi[265:288])
  dnbr <- pre-dist
  ari <- post-dist

  rrim <- ari/dnbr
  r80pm <- post/(0.8*pre)
  yryrm <- (mean(tsi[265:288]) - dist)/(mean(c(265:288)) - mean(c(217:228)))

  expect_equal(met$RRI, rrim, tolerance = 1e-4)
  expect_equal(met$R80P, r80pm, tolerance = 1e-4)
  expect_equal(met$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(met$impact, dnbr, tolerance = 1e-4)
  expect_equal(met$Vpre, pre, tolerance = 1e-4)
})

test_that("calcRecMetrics - no negative break", {

  tsi <- c(rep(1,24), seq(2, -1, length.out=72), rep(-1,24), seq(5, 0.5, length.out=72),rep(0.5,24), seq(8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 20

  selBreak <- 'first'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  expect_equal(met$RRI, NA, tolerance = 1e-4)
  expect_equal(met$R80P, NA, tolerance = 1e-4)
  expect_equal(met$YrYr, NA, tolerance = 1e-4)
  expect_equal(met$impact, NA, tolerance = 1e-4)
  expect_equal(met$Vpre, NA, tolerance = 1e-4)

  tsi <- c(rep(1,24), seq(2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)
  pre <- -1
  dist <- mean(tsi[121:132])
  post <- max(tsi[169:192])
  dnbr <- pre-dist
  ari <- post-dist

  rrim <- ari/dnbr
  r80pm <- post/(0.8*pre)
  yryrm <- (mean(tsi[169:192]) - dist)/(mean(c(169:192)) - mean(c(121:132)))

  expect_equal(met$RRI, rrim, tolerance = 1e-4)
  expect_equal(met$R80P, r80pm, tolerance = 1e-4)
  expect_equal(met$YrYr, yryrm, tolerance = 1e-4)
  expect_equal(met$impact, dnbr, tolerance = 1e-4)
  expect_equal(met$Vpre, pre, tolerance = 1e-4)

})

test_that("calcRecMetrics - no positive slope", {

  tsi <- c(rep(1,24), seq(-2, -3, length.out=72), rep(-1,24), seq(5, 0.5, length.out=72),rep(0.5,24), seq(8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 20

  selBreak <- 'first'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  expect_equal(met$RRI, NA, tolerance = 1e-4)
  expect_equal(met$R80P, NA, tolerance = 1e-4)
  expect_equal(met$YrYr, NA, tolerance = 1e-4)
  expect_equal(met$impact, NA, tolerance = 1e-4)
  expect_equal(met$Vpre, NA, tolerance = 1e-4)
})

test_that("calcRecMetrics - timing between break and disturbance", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(5, 0.5, length.out=72),rep(0.5,24), seq(8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 2

  selBreak <- 'first'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  expect_equal(met$RRI, NA, tolerance = 1e-4)
  expect_equal(met$R80P, NA, tolerance = 1e-4)
  expect_equal(met$YrYr, NA, tolerance = 1e-4)
  expect_equal(met$impact, NA, tolerance = 1e-4)
  expect_equal(met$Vpre, NA, tolerance = 1e-4)
})

test_that("calcRecMetrics - break in post-disturbance period", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(5, 0.5, length.out=72),rep(0.5,24), seq(8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 2
  nDist <- 1
  nPost <- 4
  nPostStart <- 8
  nDelta <- 4
  nDeltaStart <- 8
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 2

  selBreak <- 'first'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  expect_equal(met$RRI, NA, tolerance = 1e-4)
  expect_equal(met$R80P, NA, tolerance = 1e-4)
  expect_equal(met$YrYr, NA, tolerance = 1e-4)
  expect_equal(met$impact, NA, tolerance = 1e-4)
  expect_equal(met$Vpre, NA, tolerance = 1e-4)
})

test_that("calcRecMetrics - break in pre-disturbance period", {

  tsi <- c(rep(1,24), seq(-2, -1, length.out=72), rep(-1,24), seq(5, 0.5, length.out=72),rep(0.5,24), seq(8, 0.5, length.out=72),rep(0.5,24))
  tdist <- 125
  obspyr <- 12
  nPre<- 10
  nDist <- 1
  nPost <- 2
  nPostStart <- 4
  nDelta <- 2
  nDeltaStart <- 4
  tbp <- c(24,120,216)
  chkBrk <- T
  timeThres <- 2

  selBreak <- 'close'
  met <- calcRecMetrics(tsi, tdist, obspyr, nPre, nDist, nPost, nPostStart, nDelta, nDeltaStart,
                        tbp, selBreak, chkBrk = F, timeThres)

  expect_equal(met$RRI, NA, tolerance = 1e-4)
  expect_equal(met$R80P, NA, tolerance = 1e-4)
  expect_equal(met$YrYr, NA, tolerance = 1e-4)
  expect_equal(met$impact, NA, tolerance = 1e-4)
  expect_equal(met$Vpre, NA, tolerance = 1e-4)
})
