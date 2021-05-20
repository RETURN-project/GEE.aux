context("Generate regular time series")

test_that("Generate a regular time series", {

  dts <- as.Date(c('2000-01-01','2000-02-01','2000-03-01'))
  obsi <- c(0.85,0.63,0.52)
  main <- 'title'
  lossdt_start <- as.Date('2000-02-15')
  lossdt_end <- as.Date('2000-02-20')
  lossdt_start1 <- as.Date('2000-01-05')
  lossdt_end1 <- as.Date('2000-03-01')
  trend <- c(0.642,0.65,0.595)
  met_rri <- 0.52
  met_r80p <- 0.36
  met_yryr <- 0.02

  pl <- plot_aux(dts, obsi,main, lossdt_start,lossdt_end,lossdt_start1,lossdt_end1,lossdt_start1,lossdt_end1,
                       trend,met_rri,met_r80p,met_yryr)

  expect_equal(pl, c(), tolerance = 1e-6)
})
