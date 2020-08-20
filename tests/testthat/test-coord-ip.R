context("coord-ip")

library(ggplot2)

test_that("ipaddress classes passed through ggplot unscaled", {
  expect_equal(ggplot2::scale_type(ip_address()), "identity")
  expect_equal(ggplot2::scale_type(ip_network()), "identity")
  expect_equal(ggplot2::scale_type(ip_interface()), "identity")
})

test_that("visual tests of ip_address data", {
  addresses <- ip_address(c("0.0.0.0", "128.0.0.0", "192.168.0.1", "255.255.255.255"))

  plot_address <- ggplot(data = data.frame(ip = addresses)) +
    geom_point(aes(x = ip$x, y = ip$y), na.rm = TRUE)

  vdiffr::expect_doppelganger(
    "Addresses with default",
    plot_address + coord_ip()
  )
  vdiffr::expect_doppelganger(
    "Addresses with zoom",
    plot_address + coord_ip(canvas_network = ip_network("128.0.0.0/1"), pixel_prefix = 17)
  )
  vdiffr::expect_doppelganger(
    "Addresses with resolution",
    plot_address + coord_ip(pixel_prefix = 10)
  )
  vdiffr::expect_doppelganger(
    "Addresses with Morton curve",
    plot_address + coord_ip(curve = "morton")
  )
})

test_that("visual tests of ip_network data", {
  networks <- ip_network(c("1.0.0.0/8", "224.0.0.0/4", "12.0.0.0/16"))

  plot_address <- ggplot(data = data.frame(ip = networks)) +
    geom_rect(aes(xmin = ip$xmin, ymin = ip$ymin, xmax = ip$xmax, ymax = ip$ymax), na.rm = TRUE)

  vdiffr::expect_doppelganger(
    "Networks with default",
    plot_address + coord_ip()
  )
  vdiffr::expect_doppelganger(
    "Networks with zoom",
    plot_address + coord_ip(canvas_network = ip_network("128.0.0.0/1"), pixel_prefix = 17)
  )
  vdiffr::expect_doppelganger(
    "Networks with resolution",
    plot_address + coord_ip(pixel_prefix = 10)
  )
  vdiffr::expect_doppelganger(
    "Networks with Morton curve",
    plot_address + coord_ip(curve = "morton")
  )
})
