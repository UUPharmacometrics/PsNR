context("test-result")

test_that("capturing of results works", {
  r <- as_result(2+2)
  expect_equal(get_result(r), 4)
  expect_false(has_errors(r))
})

test_that("capturing of errors works", {
  r <- tryCatch(stop(), error = function(e) return(e)) %>% as_result()
  expect_true(has_errors(r))
})

