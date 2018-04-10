## test data read function

test_that("fars_read",{
  test_object <- fars_read_years(2013:2015)
  expect_that(test_object,is_a("list"))
})
