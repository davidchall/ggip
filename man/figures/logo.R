library(hexSticker)
library(ggip)
library(sysfonts)


ggip_logo <- function(outfile, text_color = "#f79d1e", text_font = "Ubuntu Mono",
                      line_color = "#6ee2ff", bg_color = "#000000", border_color = "#f79d1e") {

  font_add_google(text_font)

  p <- ggplot() +
    geom_hilbert_outline(curve_order = 3, color = line_color) +
    coord_ip() +
    theme_transparent()

  sticker(
    # subplot
    subplot = p, s_x = 1, s_y = 1, s_width = 2.11, s_height = 2.11,
    # package name
    package = "ggip", p_family = text_font, p_color = text_color, p_size = 8.5, p_y = 1.18,
    # border
    h_color = border_color, h_fill = bg_color,
    # output file
    filename = outfile
  )
}


ggip_logo("man/figures/logo.svg")
ggip_logo("man/figures/logo.png")
