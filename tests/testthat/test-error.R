test_that("stop_bad_aes_type() generates useful error message", {
  verify_output(test_path("test-stop-bad-aes-type.txt"), {
    stop_bad_aes_type("my_layer", "ip", "ip_address")
  })
})

test_that("stop_missing_aes() generates useful error message", {
  verify_output(test_path("test-stop-missing-aes.txt"), {
    stop_missing_aes("my_layer", "ip")
    stop_missing_aes("my_layer", c("x", "y"))
    stop_missing_aes("my_layer", c("x", "y", "ip"))
  })
})

test_that("stop_missing_coord() generates useful error message", {
  verify_output(test_path("test-stop-missing-coord.txt"), {
    stop_missing_coord()
  })
})
