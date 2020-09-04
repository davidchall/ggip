library(ggplot2)

test_that("input validation", {
  network_data <- data.frame(network = ip_network("0.0.0.0/16"))
  address_data <- data.frame(address = ip_address("0.0.0.0"))

  expect_error(
    ggplot_build(ggplot(address_data) + stat_summary_address(aes(ip = address))),
    class = "ggip_error_missing_coord"
  )
  expect_error(
    ggplot_build(ggplot(network_data) + coord_ip() + stat_summary_address(aes(ip = network))),
    class = "ggip_error_bad_aes_type"
  )

  expect_error(
    ggplot_build(ggplot(address_data) + coord_ip() + stat_summary_address()),
    class = "ggip_error_missing_aes"
  )
  expect_error(
    ggplot_build(ggplot(address_data, aes(ip = address)) + coord_ip() + stat_summary_address(inherit.aes = FALSE)),
    class = "ggip_error_missing_aes"
  )
  expect_error(
    ggplot_build(ggplot(address_data) + coord_ip() + stat_summary_address(aes(ip = ip_address("0.0.0.0")))),
    "The `ip` aesthetic of `stat_summary_address()` must map to a `data` variable.",
    fixed = TRUE
  )
  expect_error(
    ggplot_build(ggplot(address_data) + coord_ip() + stat_summary_address(aes(ip = address), fun = sum)),
    "`stat_summary_address()` must have `z` aesthetic when using `fun` argument.",
    fixed = TRUE
  )
})

test_that("data summarized onto grid", {
  dat <- data.frame(ip = ip_address(c("0.0.0.0", "0.0.0.0", "255.255.255.255")))

  full_direct <- ggplot() +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address(aes(ip = ip), data = dat)
  ret_direct <- layer_data(full_direct)

  full_inherit_data <- ggplot(dat) +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address(aes(ip = ip))
  ret_inherit_data <- layer_data(full_inherit_data)

  full_inherit_aes <- ggplot(dat, aes(ip = ip)) +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address()
  ret_inherit_aes <- layer_data(full_inherit_aes)

  expect_equal(ret_direct, ret_inherit_data)
  expect_equal(ret_direct, ret_inherit_aes)
  expect_equal(ret_direct$x, c(0, 0, 1, 1))
  expect_equal(ret_direct$y, c(0, 1, 0, 1))
  expect_equal(ret_direct$count, c(0, 2, 0, 1))
  expect_equal(ret_direct$ip_count, c(0, 1, 0, 1))
})
