library(tidyverse)

expect_curve_endpoints <- function(canvas_network, curve) {
  min_pixel_prefix <- prefix_length(canvas_network)
  max_pixel_prefix <- pmin(min_pixel_prefix + 32, max_prefix_length(canvas_network))

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
