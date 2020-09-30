test_that("input validation", {
  address_data <- data.frame(address = ip_address("0.0.0.0"))
  network_data <- data.frame(network = ip_network("0.0.0.0/16"))

  expect_error(
    print(ggplot(network_data) + geom_hilbert_outline(aes(ip = network))),
    class = "ggip_error_missing_coord"
  )
  expect_error(
    print(ggplot(address_data) + coord_ip() + geom_hilbert_outline(aes(ip = address))),
    class = "ggip_error_bad_aes_type"
  )
  expect_error(
    print(ggplot(network_data) + coord_ip(curve = "morton") + geom_hilbert_outline(aes(ip = network))),
    '`geom_hilbert_outline()` requires `coord_ip(curve = "hilbert")`.',
    fixed = TRUE
  )
})
