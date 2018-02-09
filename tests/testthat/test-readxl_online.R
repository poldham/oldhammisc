context("readxl_online")

test_that("xls case works", {
  dfxls <- readxl_online("https://github.com/wipo-analytics/opensource-patent-analytics/blob/master/2_datasets/pizza_all_24294/patentscope_pizza_1940_2005_9659.xls?raw=true")
  expect_is(dfxls, "data.frame")
})

test_that("xlsx case works", {
  dfxlsx <- readxl_online("https://github.com/wipo-analytics/opensource-patent-analytics/blob/master/2_datasets/ewaste/ewaste.xlsx?raw=true")
  expect_is(dfxlsx, "data.frame")

})

test_that("zip case works", {
  dfzip <- readxl_online("https://github.com/poldham/opensource-patent-analytics/blob/master/2_datasets/taxonomy_final.zip?raw=true")
  expect_is(dfzip, "data.frame")

})


# test_that("type argument works", {
#   type <- readxl_online("https://github.com/wipo-analytics/opensource-patent-analytics/blob/master/2_datasets/ewaste/ewaste.xlsx?raw=true", col_types = "text")
#
#
# })

# test_that("error message works", {
#   error <- readxl_online("https://www.google.co.uk/")
#   expect_error(error, "Error: Missing file extension")
#   expect_message(error, "Expecting file extension of type .xlsx, .xls or .zip in the URL. Check the URL or the data source for the correct file extension and use the type argument")
#
# })
