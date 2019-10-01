context("test-qa_results")

test_that("retrieving scm results works", {
  continuous <- c("AGE", "WT", "BMI", "HT", "SCR", "CRCL")
  categorical <- c("SEX", "RACE", "ETH", "GRP")
  parameters <- c("CLN", "V2", "V3", "CLR", "VF1", "MTT", "NN", "V4", "CLM")
  expect_silent(scm_results <- retrieve_scm_results("data/raw_results_scm.csv", parameters, continuous, categorical))
  expect_is(scm_results, "data.frame")
  expect_equal(nrow(scm_results), 117)
})
