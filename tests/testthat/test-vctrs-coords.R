test_that("coords classes passed through ggplot unscaled", {
  expect_equal(scale_type(ip_address_coords()), "identity")
  expect_equal(scale_type(ip_network_coords()), "identity")
})

test_that("coords classes identified", {
  expect_true(is_ip_address_coords(ip_address_coords()))
  expect_false(is_ip_address_coords(ip_network_coords()))
  expect_true(is_ip_network_coords(ip_network_coords()))
  expect_false(is_ip_network_coords(ip_address_coords()))
})
