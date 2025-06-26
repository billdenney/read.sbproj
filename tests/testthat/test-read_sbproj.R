test_that("read_sbproj", {
  expect_silent(
    read_sbproj(system.file("example/TfRBACE_model_ver3p5.sbproj", package = "read.sbproj"))
  )
})
