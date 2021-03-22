test_that("Adding labels to columns in a dataframe works", {
  dat <- data.frame(a = 1:3)
  dat <- set_labels(dat, "a", "Example")

  expect_equal(attributes(dat$a)$label, "Example")
})