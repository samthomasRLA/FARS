## test data read function

context("Sample test")

testthat::test_that("fars_read",{
  test_object <- fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "FARS"))
  testthat::expect_that(test_object,testthat::is_a("data.frame"))
})
