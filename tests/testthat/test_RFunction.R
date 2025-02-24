source(here("tests/testthat/helper.R"))

test_data1 <- test_data("input3_move2loc_LatLon.rds")

test_that("function runs without error", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")


  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]

      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points = 10,
        extent_type = extent_type
      ))
    }
  }
})
