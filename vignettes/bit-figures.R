library(tidyverse)
library(ipaddress)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


bit_figure <- function(address, path,
                       canvas_prefix = 0, pixel_prefix = max_prefix_length(address),
                       canvas_color = "#fed766", pixel_color = "#2ab7ca") {

  bit_array <- strsplit(ip_to_binary(address), "")[[1]]

  table_rows <- tibble(bit = bit_array) %>%
    mutate(
      is_canvas = row_number() - 1 < canvas_prefix,
      is_pixel = row_number() - 1 >= pixel_prefix,
      table_row = case_when(
        is_canvas ~ str_glue('<td border="1" bgcolor="{canvas_color}">{bit}</td>'),
        is_pixel ~ str_glue('<td border="1" bgcolor="{pixel_color}">{bit}</td>'),
        TRUE ~ str_glue('<td border="1">{bit}</td>')
      )
    ) %>%
    pull(table_row)

  graph <- str_glue('
  digraph address {{
    address [
      shape = none
      label = <<table border="0" cellspacing="0"><tr>
              {paste(table_rows, collapse = "\n")}
              </tr></table>>
    ]
  }}
  ')

  grViz(graph) %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(path, height = 100)
}


bit_figure(ip_address("192.168.0.124"), "vignettes/bits_raw.png")

bit_figure(ip_address("192.168.0.124"), "vignettes/bits_half_reduced.png", canvas_prefix = 8)

bit_figure(ip_address("192.168.0.124"), "vignettes/bits_reduced.png", canvas_prefix = 8, pixel_prefix = 24)

