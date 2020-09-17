test_that("ipaddress classes passed through ggplot unscaled", {
  expect_equal(scale_type(ip_address()), "identity")
  expect_equal(scale_type(ip_network()), "identity")
  expect_equal(scale_type(ip_interface()), "identity")
})

test_that("coords classes passed through ggplot unscaled", {
  expect_equal(scale_type(ip_address_coords()), "identity")
  expect_equal(scale_type(ip_network_coords()), "identity")
})
