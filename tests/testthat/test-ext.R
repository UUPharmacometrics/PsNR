context("test-ext")

ext_list <- read_nm_ext("data/sim-5.ext")

test_that("Obtaining final OFV values works", {
  expect_equal(get_final_ofvs(ext_list), 598.36671837314225)
  
  expect_equal(get_final_ofvs(ext_list, 1:3), 
               c(598.36671837314225, 550.45030358567010, 547.04485076708750))
})
