test_that("Label checking works.", {
  v <- 1:3
  attr(v, "label") <- "Example"
  expect_true(is_labelled(v))
})