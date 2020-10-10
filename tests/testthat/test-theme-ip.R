test_that("themes drop elements", {
  expect_simplistic_theme <- function(t) {
    expect_s3_class(t$axis.text, "element_blank")
    expect_s3_class(t$axis.ticks, "element_blank")
    expect_s3_class(t$axis.title, "element_blank")
    expect_s3_class(t$panel.border, "element_blank")
    expect_s3_class(t$panel.grid, "element_blank")
  }

  expect_simplistic_theme(theme_ip_light())
  expect_simplistic_theme(theme_ip_dark())
})
