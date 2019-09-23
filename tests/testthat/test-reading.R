context("test-reading")

test_that("Reading of standard NM tables works", {
  tab <- read_nm_std_table("data/run7_linbase.dta")
  expect_is(tab, "data.frame")
  expect_equal(colnames(tab), c("ID", "DV", "MDV", "OPRED", "H011", "TIME", "AMT", 
                                "WGT", "APGR",  "G011", "G021", "ETA1", "ETA2", 
                                "D_EPSETA1_1", "D_EPSETA1_2"))
  expect_equal(nrow(tab), 744)
})
