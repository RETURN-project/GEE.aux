Sys.setenv("R_TESTS" = "")
library(testthat)
library(GEE.aux)

test_check("GEE.aux")
