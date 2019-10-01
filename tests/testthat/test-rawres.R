context("test-rawres")

test_that("reading of rawres files works", {
  tab <- read_psn_rawres("data/raw_results_simeval.csv")
  expect_is(tab, "data.frame")
})
