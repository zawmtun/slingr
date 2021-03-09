test_that("flow_folders() can create folders.", {
  temp_dir <- tempdir(check = TRUE)
  flow_folders(temp_dir)

  expect_true(dir.exists(file.path(temp_dir, "code")))
})
