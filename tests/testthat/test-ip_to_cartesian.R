library(tidyverse)

expect_curve_endpoints <- function(canvas_network, curve) {
  max_plotted_bits <- 24
  min_pixel_prefix <- prefix_length(canvas_network)
  max_pixel_prefix <- pmin(min_pixel_prefix + max_plotted_bits, max_prefix_length(canvas_network))

  curve_orders <- tibble(
    canvas_network,
    pixel_prefix = seq.int(min_pixel_prefix, max_pixel_prefix, 2),
    curve_order = (pixel_prefix - prefix_length(canvas_network)) / 2
  )

  result <- curve_orders %>%
    rowwise() %>%
    mutate(
      start = address_to_cartesian(network_address(canvas_network), canvas_network, pixel_prefix, curve),
      end = address_to_cartesian(broadcast_address(canvas_network), canvas_network, pixel_prefix, curve)
    ) %>%
    ungroup()

  expected <- curve_orders %>%
    mutate(
      start = data.frame(x = 0, y = 2 ^ curve_order - 1),
      end = data.frame(
        x = 2 ^ curve_order - 1,
        y = if (curve == "hilbert") 2 ^ curve_order - 1 else 0
      )
    )

  expect_equal(result, expected)
}

test_that("Start and end points", {
  expect_curve_endpoints(ip_network("0.0.0.0/0"), "hilbert")
  expect_curve_endpoints(ip_network("224.0.0.0/4"), "hilbert")
  expect_curve_endpoints(ip_network("::/0"), "hilbert")
  expect_curve_endpoints(ip_network("2001:db8::/32"), "hilbert")

  expect_curve_endpoints(ip_network("0.0.0.0/0"), "morton")
  expect_curve_endpoints(ip_network("224.0.0.0/4"), "morton")
  expect_curve_endpoints(ip_network("::/0"), "morton")
  expect_curve_endpoints(ip_network("2001:db8::/32"), "morton")
})

test_that("Addresses mapped to points", {

})

test_that("Networks mapped to bounding boxes", {
  # smaller than pixel
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/17"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 191, ymin = 192, xmax = 191, ymax = 192)
  )
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/17"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 128, ymin = 63, xmax = 128, ymax = 63)
  )

  # square
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/4"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 128, ymin = 192, xmax = 191, ymax = 255)
  )
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/4"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 128, ymin = 0, xmax = 191, ymax = 63)
  )

  # rectangle
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/3"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 128, ymin = 192, xmax = 255, ymax = 255)
  )
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/3"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 128, ymin = 0, xmax = 255, ymax = 63)
  )
})

test_that("Outside canvas mapped to NA", {
  expect_equal(
    address_to_cartesian(ip_address("0.0.0.0"), ip_network("224.0.0.0/4")),
    data.frame(x = NA_integer_, y = NA_integer_)
  )
  expect_equal(
    address_to_cartesian(ip_address("::"), ip_network("2001:db8::/32"), pixel_prefix = 48),
    data.frame(x = NA_integer_, y = NA_integer_)
  )
  expect_equal(
    address_to_cartesian(ip_address("::"), ip_network("224.0.0.0/4")),
    data.frame(x = NA_integer_, y = NA_integer_)
  )
  expect_equal(
    network_to_cartesian(ip_network("0.0.0.0/32"), ip_network("224.0.0.0/4")),
    data.frame(xmin = NA_integer_, ymin = NA_integer_, xmax = NA_integer_, ymax = NA_integer_)
  )
  expect_equal(
    network_to_cartesian(ip_network("::/32"), ip_network("2001:db8::/32"), pixel_prefix = 48),
    data.frame(xmin = NA_integer_, ymin = NA_integer_, xmax = NA_integer_, ymax = NA_integer_)
  )
  expect_equal(
    network_to_cartesian(ip_network("224.0.0.0/3"), ip_network("224.0.0.0/4")),
    data.frame(xmin = NA_integer_, ymin = NA_integer_, xmax = NA_integer_, ymax = NA_integer_)
  )
  expect_equal(
    network_to_cartesian(ip_network("::/32"), ip_network("224.0.0.0/4")),
    data.frame(xmin = NA_integer_, ymin = NA_integer_, xmax = NA_integer_, ymax = NA_integer_)
  )
})

test_that("input validation of mapping parameters", {
  valid1 <- ip_network("0.0.0.0/0")
  valid2 <- 16
  valid3 <- "hilbert"

  expect_error(
    validate_mapping_params(ip_address("0.0.0.0"), valid2, valid3),
    "`canvas_network` must be an ip_network scalar"
  )
  expect_error(
    validate_mapping_params(ip_network(rep("0.0.0.0/0", 2)), valid2, valid3),
    "`canvas_network` must be an ip_network scalar"
  )
  expect_error(
    validate_mapping_params(valid1, 2.5, valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_mapping_params(valid1, c(1, 2), valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_mapping_params(valid1, -1, valid3),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    validate_mapping_params(valid1, valid2, "hilber"),
    "`curve` must be one of"
  )
  expect_silent(validate_mapping_params(ip_network("0.0.0.0/16"), 32, valid3))
  expect_error(
    validate_mapping_params(ip_network("0.0.0.0/16"), 33, valid3),
    "`pixel_prefix` cannot be greater than 32 for IPv4"
  )
  expect_silent(validate_mapping_params(ip_network("::/120"), 128, valid3))
  expect_error(
    validate_mapping_params(ip_network("::/120"), 129, valid3),
    "`pixel_prefix` cannot be greater than 128 for IPv6"
  )
  expect_error(
    validate_mapping_params(ip_network("0.0.0.0/16"), 14, valid3),
    "`pixel_prefix` must be greater than prefix_length(`canvas_network`)",
    fixed = TRUE
  )
  expect_error(
    validate_mapping_params(valid1, 31, valid3),
    "The difference between prefix_length(`canvas_network`) and `pixel_prefix` cannot be odd",
    fixed = TRUE
  )
  expect_error(
    validate_mapping_params(valid1, 32, valid3),
    "Current parameters would result in plot with 65536x65536 pixels"
  )
})

test_that("Missing values", {
  expect_equal(
    address_to_cartesian(ip_address(NA)),
    data.frame(x = NA_integer_, y = NA_integer_)
  )
  expect_equal(
    network_to_cartesian(ip_network(NA)),
    data.frame(xmin = NA_integer_, ymin = NA_integer_, xmax = NA_integer_, ymax = NA_integer_)
  )
  expect_error(
    address_to_cartesian(ip_address("0.0.0.0"), canvas_network = ip_network(NA)),
    "`canvas_network` cannot be NA"
  )
  expect_error(
    network_to_cartesian(ip_network("0.0.0.0/32"), canvas_network = ip_network(NA)),
    "`canvas_network` cannot be NA"
  )
  expect_error(
    address_to_cartesian(ip_address("0.0.0.0"), pixel_prefix = NA),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    network_to_cartesian(ip_network("0.0.0.0/32"), pixel_prefix = NA),
    "`pixel_prefix` must be a positive integer scalar"
  )
  expect_error(
    address_to_cartesian(ip_address("0.0.0.0"), curve = NA_character_),
    "`curve` must be a string"
  )
  expect_error(
    network_to_cartesian(ip_network("0.0.0.0/32"), curve = NA_character_),
    "`curve` must be a string"
  )
})
