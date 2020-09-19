library(dplyr)

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
      start = data.frame(x = 0, y = 2^curve_order - 1),
      end = data.frame(
        x = 2^curve_order - 1,
        y = if (curve == "hilbert") 2^curve_order - 1 else 0
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
  expect_equal(
    address_to_cartesian(ip_address("224.0.0.0"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(x = 191, y = 192)
  )
  expect_equal(
    address_to_cartesian(ip_address("224.0.0.0"), pixel_prefix = 16, curve = "morton"),
    data.frame(x = 128, y = 63)
  )
  expect_equal(
    address_to_cartesian(ip_address("fc00::"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(x = 255, y = 224)
  )
  expect_equal(
    address_to_cartesian(ip_address("fc00::"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "morton"),
    data.frame(x = 224, y = 31)
  )
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
  expect_equal(
    network_to_cartesian(ip_network("fc00::/17"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 255, ymin = 224, xmax = 255, ymax = 224)
  )
  expect_equal(
    network_to_cartesian(ip_network("fc00::/17"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 224, ymin = 31, xmax = 224, ymax = 31)
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
  expect_equal(
    network_to_cartesian(ip_network("fc00::/8"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 240, ymin = 224, xmax = 255, ymax = 239)
  )
  expect_equal(
    network_to_cartesian(ip_network("fc00::/8"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 224, ymin = 16, xmax = 239, ymax = 31)
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
  expect_equal(
    network_to_cartesian(ip_network("fc00::/7"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "hilbert"),
    data.frame(xmin = 224, ymin = 224, xmax = 255, ymax = 239)
  )
  expect_equal(
    network_to_cartesian(ip_network("fc00::/7"), canvas_network = ip_network("::/0"), pixel_prefix = 16, curve = "morton"),
    data.frame(xmin = 224, ymin = 16, xmax = 255, ymax = 31)
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

test_that("Input validation of mapping parameters", {
  verify_output(test_path("test-stop-bad-coord.txt"), {
    validate_mapping_params(ip_address("0.0.0.0"), 16)
    validate_mapping_params(ip_network(rep("0.0.0.0/0", 2)), 16)

    validate_mapping_params(ip_network("0.0.0.0/0"), 2.5)
    validate_mapping_params(ip_network("0.0.0.0/0"), c(1, 2))
    validate_mapping_params(ip_network("0.0.0.0/0"), -1)

    address_to_cartesian(ip_address("0.0.0.0"), curve = "hilber")
    network_to_cartesian(ip_network("0.0.0.0/0"), curve = "hilber")

    validate_mapping_params(ip_network("0.0.0.0/16"), 33)
    validate_mapping_params(ip_network("::/120"), 129)
    validate_mapping_params(ip_network("0.0.0.0/16"), 14)
    validate_mapping_params(ip_network("0.0.0.0/0"), 31)
    validate_mapping_params(ip_network("0.0.0.0/0"), 32)
  })
})

test_that("Other input validation", {
  expect_error(address_to_cartesian(ip_network()), "`address` must be a vector with type <ip_address>.")
  expect_error(network_to_cartesian(ip_address()), "`network` must be a vector with type <ip_network>.")
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
})
