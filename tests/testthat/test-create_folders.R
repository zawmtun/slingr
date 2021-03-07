test_that("create_folders() can create folders", {
  temp_dir <- tempdir(check = TRUE)
  create_folders(temp_dir)

  expect_true(dir.exists(file.path(temp_dir, "code")))
})
