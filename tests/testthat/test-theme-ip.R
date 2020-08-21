context("theme-ip")

library(ggplot2)

test_that("visual tests", {
  plot <- ggplot(data = data.frame(ip = ip_address("128.0.0.0"))) +
    geom_point(aes(x = ip$x, y = ip$y), color = "grey") +
    coord_ip()

  vdiffr::expect_doppelganger("theme light", plot + theme_ip_light())
  vdiffr::expect_doppelganger("theme dark", plot + theme_ip_dark())
})
