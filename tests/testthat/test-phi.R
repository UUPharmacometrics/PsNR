context("test-phi")
lst <- read_nm_phi("data/sim-5.phi")

test_that("getting of iOFVs works", {
  expect_equal(get_iofv_sum(lst, 1:3), c(598.366718373142, 550.45030358567, 547.044850767087))
})
