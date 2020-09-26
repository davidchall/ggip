test_that("input validation", {
  network_data <- data.frame(network = ip_network("0.0.0.0/16"))
  address_data <- data.frame(address = ip_address("0.0.0.0"))

  expect_error(
    ggplot_build(ggplot(address_data) +
      stat_summary_address(aes(ip = address))),
    class = "ggip_error_missing_coord"
  )
  expect_error(
    ggplot_build(ggplot(network_data) +
      coord_ip() +
      stat_summary_address(aes(ip = network))),
    class = "ggip_error_bad_aes_type"
  )

  expect_error(
    ggplot_build(ggplot(address_data) +
      coord_ip() +
      stat_summary_address()),
    class = "ggip_error_missing_aes"
  )
  expect_error(
    ggplot_build(ggplot(address_data, aes(ip = address)) +
      coord_ip() +
      stat_summary_address(inherit.aes = FALSE)),
    class = "ggip_error_missing_aes"
  )
  expect_error(
    ggplot_build(ggplot(address_data) +
      coord_ip() +
      stat_summary_address(aes(ip = ip_address("0.0.0.0")))),
    "The `ip` aesthetic of `stat_summary_address()` must map to a `data` variable.",
    fixed = TRUE
  )
  expect_error(
    ggplot_build(ggplot(address_data) +
      coord_ip() +
      stat_summary_address(aes(ip = address), fun = sum)),
    "`stat_summary_address()` must have `z` aesthetic when using `fun` argument.",
    fixed = TRUE
  )
})

test_that("alternative ways to specify data/aesthetics", {
  dat <- data.frame(
    ip = ip_address(c("0.0.0.0", "0.0.0.0", "255.255.255.255"))
  )

  p1 <- ggplot() +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address(aes(ip = ip), data = dat)

  p2 <- ggplot(dat) +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address(aes(ip = ip))

  p3 <- ggplot(dat, aes(ip = ip)) +
    coord_ip(pixel_prefix = 2) +
    stat_summary_address()

  expect_equal(layer_data(p1), layer_data(p2))
  expect_equal(layer_data(p1), layer_data(p3))

  expect_equal(layer_data(p1)$x, c(0, 0, 1, 1))
  expect_equal(layer_data(p1)$y, c(0, 1, 0, 1))
  expect_equal(layer_data(p1)$count, c(0, 2, 0, 1))
})

test_that("alternative ways to specify summary function", {
  dat <- data.frame(
    ip = ip_address(c("0.0.0.0", "0.0.0.0", "255.255.255.255")),
    z = c(1, 2, 3)
  )

  p_base <- ggplot(dat, aes(ip = ip, z = z)) +
    coord_ip(pixel_prefix = 2)

  p1 <- p_base + stat_summary_address(fun = "mean")
  p2 <- p_base + stat_summary_address(fun = mean)
  p3 <- p_base + stat_summary_address(fun = ~ mean(.x))

  expect_equal(layer_data(p1), layer_data(p2))
  expect_equal(layer_data(p1), layer_data(p3))
})

test_that("addresses outside 2D grid raise warning", {
  dat <- data.frame(ip = ip_address(c("0.0.0.0", "255.255.255.255")))

  p <- ggplot(dat, aes(ip = ip)) +
    coord_ip(
      canvas_network = ip_network("0.0.0.0/2"),
      pixel_prefix = 2
    )

  expect_warning(layer_data(p + stat_summary_address()))
  expect_silent(layer_data(p + stat_summary_address(na.rm = TRUE)))
})
