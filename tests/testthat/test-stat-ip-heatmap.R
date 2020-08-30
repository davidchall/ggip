library(ggplot2)

test_that("input validation", {
  network_data <- data.frame(network = ip_network("0.0.0.0/16"))
  address_data <- data.frame(address = ip_address("0.0.0.0"))

  # fails when coord_ip() not used
  # TODO: how can we catch this to produce helpful error message?
  expect_error(
    ggplot_build(ggplot(address_data) + stat_ip_heatmap(aes(ip = address))),
  )

  expect_error(
    ggplot_build(ggplot(network_data) + stat_ip_heatmap(aes(ip = network)) + coord_ip()),
    "stat_ip_heatmap requires `ip` aesthetic to be an ip_address vector"
  )

  expect_error(
    ggplot_build(ggplot(address_data, aes(ip = address)) + stat_ip_heatmap() + coord_ip()),
    "stat_ip_heatmap requires `ip` aesthetic"
  )

  expect_error(
    ggplot_build(ggplot(address_data) + stat_ip_heatmap(aes(ip = address), fun = sum) + coord_ip()),
    "stat_ip_heatmap requires `z` aesthetic when using non-default summary function"
  )
})
