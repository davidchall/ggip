library(ggplot2)

test_that("input validation", {
  network_data <- data.frame(network = ip_network("0.0.0.0/16"))
  address_data <- data.frame(address = ip_address("0.0.0.0"))

  # fails when coord_ip() not used
  # TODO: how can we catch this to produce helpful error message?
  expect_error(
    ggplot_build(ggplot(address_data) + stat_summary_address(aes(ip = address))),
  )

  expect_error(
    ggplot_build(ggplot(network_data) + stat_summary_address(aes(ip = network)) + coord_ip()),
    "stat_summary_address requires `ip` aesthetic to be an ip_address vector"
  )

  expect_error(
    ggplot_build(ggplot(address_data, aes(ip = address)) + stat_summary_address() + coord_ip()),
    "stat_summary_address requires `ip` aesthetic"
  )

  expect_error(
    ggplot_build(ggplot(address_data) + stat_summary_address(aes(ip = address), fun = sum) + coord_ip()),
    "stat_summary_address requires `z` aesthetic when using non-default summary function"
  )
})
