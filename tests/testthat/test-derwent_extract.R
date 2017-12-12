context("derwent_extract")

test_that("number extract works", {
  num <- derwent_extract(five_texts, "text", section = "number")
  expect_named(num, "doc_id", "text", "num")

})

test_that("title extract works", {
  num <- derwent_extract(five_texts, "text", section = "title")
  expect_named(num, "doc_id", "text", "title")

})

test_that("abstract extract works", {
  num <- derwent_extract(five_texts, "text", section = "abstract")
  expect_named(num, "doc_id", "text", "abstract")

})


test_that("description extract works", {
  num <- derwent_extract(five_texts, "text", section = "description")
  expect_named(num, "doc_id", "text", "description")

})


test_that("claims extract works", {
  num <- derwent_extract(five_texts, "text", section = "claims")
  expect_named(num, "doc_id", "text", "claims")

})

test_that("tac extract works", {
  num <- derwent_extract(five_texts, "text", section = "tac")
  expect_named(num, "doc_id", "text", "title", "abstract", "claims")

})


test_that("full extract works", {
  num <- derwent_extract(five_texts, "text", section = "all")
  expect_named(num, "doc_id", "text", "number", "title", "abstract", "description", "claims")

})


