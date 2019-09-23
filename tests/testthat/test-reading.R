context("test-reading")

test_that("Reading of standard NM tables works", {
  # this a standard NONMEM output table
  tab <- read_nm_std_table("data/run7_linbase.dta")
  expect_is(tab, "data.frame")
  expect_equal(colnames(tab), c("ID", "DV", "MDV", "OPRED", "H011", "TIME", "AMT", 
                                "WGT", "APGR",  "G011", "G021", "ETA1", "ETA2", 
                                "D_EPSETA1_1", "D_EPSETA1_2"))
  expect_equal(nrow(tab), 744)
  
  #this is a table where the headers have been manipulated by PsN
  
  tab <- read_nm_std_table("data/extra_table")
  expect_is(tab, "data.frame")
  expect_equal(colnames(tab), c("ID", "CWRES", "PRED", "CIPREDI", "CPRED", "TIME", "TAD", "MDV" ))
  expect_equal(nrow(tab), 744)
  
})

test_that("Reading of standard NM simulation tables works", {
  # this a standard NONMEM output table
  tab <- read_nm_std_sim_table("data/sim_res_table-1.dta")
  expect_is(tab, "data.frame")
  expect_equal(colnames(tab), c("ID", "DV", "MDV", "CWRES", "IPRED"))
  expect_equal(nrow(tab), 9300)
})