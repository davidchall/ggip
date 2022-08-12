test_that("stop_bad_aes_type() generates useful error message", {
  expect_snapshot_error(stop_bad_aes_type("my_layer", "ip", "ip_address"))
})

test_that("stop_missing_aes() generates useful error message", {
  expect_snapshot_error(stop_missing_aes("my_layer", "ip"))
  expect_snapshot_error(stop_missing_aes("my_layer", c("x", "y")))
  expect_snapshot_error(stop_missing_aes("my_layer", c("x", "y", "ip")))
})

test_that("stop_missing_coord() generates useful error message", {
  expect_snapshot_error(stop_missing_coord())
})
