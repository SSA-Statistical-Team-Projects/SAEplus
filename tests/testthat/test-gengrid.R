
cmr.grid <- gengrid(dsn = "../testdata")

test_that("produces expected basic output with standard model defaults", {

  out <- c(length(cmr.grid), sum(cmr.grid$polygon_dt$population, na.rm = TRUE))
  expectation <- c(3, cmr.grid$total_popsize)

  expect_equal(out, expectation)

})

