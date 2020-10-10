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

test_that("alternative ways to specify data/aesthetics", {
  dat <- data.frame(
    ip = ip_network(c("0.0.0.0/2", "128.0.0.0/4"))
  )

  p1 <- ggplot() +
    coord_ip() +
    geom_hilbert_outline(aes(ip = ip), data = dat)

  p2 <- ggplot(dat) +
    coord_ip() +
    geom_hilbert_outline(aes(ip = ip))

  p3 <- ggplot(dat, aes(ip = ip)) +
    coord_ip() +
    geom_hilbert_outline()

  g1 <- layer_grob(p1)[[1]]
  g2 <- layer_grob(p2)[[1]]
  g3 <- layer_grob(p3)[[1]]

  expect_s3_class(g1, "segments")
  expect_s3_class(g2, "segments")
  expect_s3_class(g3, "segments")

  expect_equal(g1$x0, g2$x0)
  expect_equal(g1$x0, g3$x0)
  expect_equal(g1$y0, g2$y0)
  expect_equal(g1$y0, g3$y0)
  expect_equal(g1$x1, g2$x1)
  expect_equal(g1$x1, g3$x1)
  expect_equal(g1$y1, g2$y1)
  expect_equal(g1$y1, g3$y1)
})
