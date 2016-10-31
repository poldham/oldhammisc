context("testing orcid_rename")

test_that("orcid_rename input length is 45", {
  expect_length(orcid_sample, 45)
  })

test_that("orcid_rename output length is 45", {
  expect_length(orcid_rename_output, 45)
}) # works ok

test_that("orcid_rename output has no hyphen", {
  stringr::str_detect(names(orcid_rename_output), "z")
}) # this test is not working

stringr::str_detect(process_orcid_1695, "_")
#better to turn these into live tests that call rorcid, then test the input data. Then test the output data. See the rcrossref tests for examples of that.

