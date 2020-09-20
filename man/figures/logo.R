library(hexSticker)
library(ggip)
library(sysfonts)


ggip_logo <- function(outfile,
                      text_color = "#ffea00", line_color = "#7dc06f", bg_color = "#3f1f5f",
                      text_font = "Ubuntu Mono") {

  font_add_google(text_font)

  p <- ggplot() +
    geom_hilbert_outline(curve_order = 3, color = line_color) +
    coord_ip() +
    theme(panel.background = element_rect(fill = bg_color))

  sticker(
    # subplot
    subplot = p, s_x = 1, s_y = 1, s_width = 2.11, s_height = 2.11,
    # package name
    package = "ggip", p_family = text_font, p_color = text_color, p_size = 8.5, p_y = 1.18,
    # border
    h_color = "#595959", white_around_sticker = TRUE,
    # output file
    filename = outfile
  )
}


ggip_logo("man/figures/logo.svg")
ggip_logo("man/figures/logo.png")
