context("Fire data preparation")

test_that("extract date from CCI fire image name", {
  x <- '20140901-ESACCI-L3S_FIRE-BA-MODIS-AREA_2-fv51-JD_b1'
  dt <- 20140901
  extracted <- extract_date_fire(x)

  expect_equal(dt, extracted, tolerance = 1e-6)
})
