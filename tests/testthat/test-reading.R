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

test_that("Reading of ext files works",{
  ext_lst <- read_nm_ext("data/sim-5.ext")
  expect_length(ext_lst, 60)
  expect_equal(names(ext_lst)[1], "TABLE NO.     1: First Order Conditional Estimation with Interaction (Evaluation): Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=1 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0")
  expect_equal(colnames(ext_lst[[1]]), c("ITERATION", "SIGMA(1,1)", "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)",  "OBJ"))
  expect_equal(NROW(ext_lst[[1]]), 3)
})