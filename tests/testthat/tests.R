context("make_filename")

test_that("make_filename makes correct file name", {
  expect_equal(make_filename(2013),"accident_2013.csv.bz2")
  expect_equal(make_filename(2015),"accident_2015.csv.bz2")
})

context("fars_summarize_years")

test_that("fars_summarize_years obtains correct summary", {

  oldwd <- getwd()
  setwd(system.file("extdata", package = "funcFARS"))

  expect_equal(fars_summarize_years(2014)$`2014`[1], 2168)
  expect_equal(fars_summarize_years(2014)$`2014`[3], 2245)

  setwd(oldwd)

})
