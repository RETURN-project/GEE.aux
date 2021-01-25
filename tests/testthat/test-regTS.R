context("Generate regular time series")

test_that("Generate a regular time series", {
  # Generate a time series (straight line of slope m and intercept c)
  tsi <- c(5,3,8,4,2)
  dts <- as.Date(c('2001-02-03', '2001-05-04','2001-07-03','2001-07-05','2002-09-22'))

  monmax <- toRegularTS(tsi, dts, 'max', 'monthly')
  monmean <- toRegularTS(tsi, dts, 'mean', 'monthly')
  monmedian <- toRegularTS(tsi, dts, 'median', 'monthly')
  quartmax <- toRegularTS(tsi, dts, 'max', 'quart')
  quartmean <- toRegularTS(tsi, dts, 'mean', 'quart')
  quartmedian <- toRegularTS(tsi, dts, 'median', 'quart')
  dts <- as.Date(c('2001-02-03', '2001-02-04','2001-02-09','2001-02-15','2001-02-22'))
  dail <- toRegularTS(tsi, dts, 'max', 'daily')

  expect_equal(as.numeric(monmax), c(5, NA, NA, 3, NA,8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2), tolerance = 1e-6)
  expect_equal(as.numeric(monmean), c(5, NA, NA, 3, NA,6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2), tolerance = 1e-6)
  expect_equal(as.numeric(monmedian), c(5, NA, NA, 3, NA,6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2), tolerance = 1e-6)
  expect_equal(as.numeric(quartmax), c(5,  3,  8, NA, NA, NA,  2))
  expect_equal(as.numeric(quartmean), c(5,  3,  6, NA, NA, NA,  2))
  expect_equal(as.numeric(quartmedian), c(5,  3,  6, NA, NA, NA,  2))
  expect_equal(as.numeric(dail), c(5,3, NA, NA, NA, NA, 8, NA, NA, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, 2))
})

test_that("Generate a regular time series with missing date values", {
  # Generate a time series (straight line of slope m and intercept c)
  tsi <- c(5,3,8,4,2,5)
  dts <- c('20010203', '20010504','20010703','20010705','20020922', NA)

  monmean <- getRegularTS(dts,tsi, fun = 'mean', resol ='monthly')

  expect_equal(as.numeric(monmean), c(5, NA, NA, 3, NA,6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 2), tolerance = 1e-6)
  })


