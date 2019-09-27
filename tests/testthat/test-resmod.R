context("test-resmod")

test_that("reading resmod results works", {
  tab <- read_resmod_table("data/resmod_results.csv")
  expect_is(tab, "data.frame")
  expect_equal(colnames(tab), c("iteration", "dvid", "model", "dofv", "prms"))
})
