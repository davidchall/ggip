test_that("input validation of coord_ip() parameters", {
  valid1 <- ip_network("0.0.0.0/0")
  valid2 <- 16
  valid3 <- "hilbert"

  expect_error(
    validate_coord_params(ip_address("0.0.0.0"), valid2, valid3),
    "`network` must be an ip_network scalar"
  )
  expect_error(
    validate_coord_params(ip_network(rep("0.0.0.0/0", 2)), valid2, valid3),
    "`network` must be an ip_network scalar"
  )
  expect_error(
    validate_coord_params(valid1, 2.5, valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_coord_params(valid1, c(1, 2), valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_coord_params(valid1, -1, valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_coord_params(valid1, valid2, "hilber"),
    "`curve` must be one of"
  )
  expect_silent(validate_coord_params(ip_network("0.0.0.0/16"), 32, valid3))
  expect_error(
    validate_coord_params(ip_network("0.0.0.0/16"), 33, valid3),
    "`pixel_prefix` cannot be greater than 32 for IPv4"
  )
  expect_silent(validate_coord_params(ip_network("::/120"), 128, valid3))
  expect_error(
    validate_coord_params(ip_network("::/120"), 129, valid3),
    "`pixel_prefix` cannot be greater than 128 for IPv6"
  )
  expect_error(
    validate_coord_params(ip_network("0.0.0.0/16"), 14, valid3),
    "`pixel_prefix` must be greater than prefix_length(`network`)",
    fixed = TRUE
  )
  expect_error(
    validate_coord_params(valid1, 31, valid3),
    "The difference between prefix_length(`network`) and `pixel_prefix` cannot be odd",
    fixed = TRUE
  )
  expect_error(
    validate_coord_params(valid1, 32, valid3),
    "Current parameters would result in plot with 65536x65536 pixels"
  )
})

test_that("ipaddress classes passed through ggplot unscaled", {
  expect_equal(ggplot2::scale_type(ip_address()), "identity")
  expect_equal(ggplot2::scale_type(ip_network()), "identity")
  expect_equal(ggplot2::scale_type(ip_interface()), "identity")
})
