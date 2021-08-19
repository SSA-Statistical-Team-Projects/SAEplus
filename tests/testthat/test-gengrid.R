
cmr.grid <- gengrid(dsn = paste(getwd(), "data-raw", sep = "/"))

test_that("produces expected basic output with standard model defaults", {

  out <- c(length(gin.grid), sum(gin.grid$polygon_dt$population, na.rm = TRUE))
  expectation <- c(3, gin.grid$total_popsize)

  expect_equal(out, expectation)

})

ginshp <- st_read(dsn = paste(getwd(), "data-raw", sep = "/"), layer = "sous_prefectures")

gin.grid <- gengrid(layer = ginshp,
                    stats = sum,
                    raster_tif = "gin_ppp_2020_UNadj_constrained",
                    crs = 4326)

test_that("does gengrid work if the shapefile is loaded first as an object of class 'sf'", {



})
