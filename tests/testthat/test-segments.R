context("Recovery indicator: YrYr")

test_that("Straight line time series", {
  tsi <- c(rep(1,48), seq(-2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  noise <- runif(336)/10
  tsio <- tsi + noise
  obspyr <- 12

  seg <- getSegments(tsio, obspyr, h = 0.15, seas = T)

  expect_equal(seg$breakpoints,c(48,144,240), tolerance = 5)
})
