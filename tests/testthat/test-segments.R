context("Recovery indicator: YrYr")

test_that("Time series with breaks", {
  tsi <- c(rep(1,48), seq(-2, -1, length.out=72), rep(-1,24), seq(-5, 0.5, length.out=72),rep(0.5,24), seq(-8, 0.5, length.out=72),rep(0.5,24))
  noise <- runif(336)/10
  tsio <- tsi + noise
  obspyr <- 12

  seg <- getSegments(tsio, obspyr, h = 0.15, seas = T)

  expect_equal(seg$breakpoints,c(48,144,240), tolerance = 5)
})

test_that("Too many missing values", {
  tsi <- rep(NA,336)
  tsi[c(100,120,200)] <- c(1,1.2,0.8)
  obspyr <- 12

  seg <- getSegments(tsi, obspyr, h = 0.15, seas = T)

  expect_equal(seg$breakpoints,NA, tolerance = 5)
  expect_equal(seg$trend,NA, tolerance = 5)
  expect_equal(seg$loglik,NA, tolerance = 5)
  expect_equal(seg$AIC,NA, tolerance = 5)
})

test_that("No breakpoints", {
  tsi <- rep(1,336)
  noise <- runif(336)/10
  tsio <- tsi + noise
  obspyr <- 12

  seg <- getSegments(tsio, obspyr, h = 0.15, seas = T)

  expect_equal(seg$breakpoints,NA, tolerance = 5)
  expect_equal(is.na(seg$trend[1]),FALSE, tolerance = 5)
})
