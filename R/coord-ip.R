#' Coordinate system for IP data
#'
#' @description
#' A ggplot2 coordinate system that maps a range of IP address space onto a
#' two-dimensional grid using a space-filling curve.
#'
#' `coord_ip()` forms the foundation of any ggip plot. It translates all
#' [`ip_address`][`ipaddress::ip_address`] and [`ip_network`][`ipaddress::ip_network`]
#' vectors to Cartesian coordinates, ready for use by ggplot2 layers (see
#' Accessing Coordinates). This ensures all layers use a common mapping.
#'
#' @section Accessing Coordinates:
#' `coord_ip()` stores the result of the mapping in a nested data frame column.
#' This means each [`ip_address`][`ipaddress::ip_address`] or
#' [`ip_network`][`ipaddress::ip_network`] column in the original data set is
#' converted to a data frame column. When specifying ggplot2 aesthetics, you'll
#' need to use `$` to access the nested data (see Examples).
#'
#' Each [`ip_address`][`ipaddress::ip_address`] column will be replaced with a
#' data frame containing the following columns:
#'
#' | Column name | Data type    | Description      |
#' |:------------|:-------------|:-----------------|
#' | `ip`        | `ip_address` | Original IP data |
#' | `x`         | `integer`    | Pixel x          |
#' | `y`         | `integer`    | Pixel y          |
#'
#' Each [`ip_network`][`ipaddress::ip_network`] column will be replaced with a
#' data frame containing the following columns:
#'
#' | Column name | Data type    | Description       |
#' |:------------|:-------------|:------------------|
#' | `ip`        | `ip_network` | Original IP data  |
#' | `xmin`      | `integer`    | Bounding box xmin |
#' | `ymin`      | `integer`    | Bounding box ymin |
#' | `xmax`      | `integer`    | Bounding box xmax |
#' | `ymax`      | `integer`    | Bounding box ymax |
#'
#' @inheritParams ip_to_cartesian
#' @param expand If `TRUE`, adds a small expanded margin around the data grid.
#'   The default is `FALSE`.
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#'
#' tibble(address = ip_address(c("0.0.0.0", "128.0.0.0", "192.168.0.1"))) %>%
#'   ggplot(aes(x = address$x, y = address$y, label = address$ip)) +
#'   geom_point() +
#'   geom_label(nudge_x = c(10, 0, -10), nudge_y = -10) +
#'   coord_ip(expand = TRUE) +
#'   theme_ip_light()
#'
#' tibble(network = ip_network(c("0.0.0.0/8", "224.0.0.0/4"))) %>%
#'   mutate(
#'     start = network_address(network),
#'     end = broadcast_address(network)
#'   ) %>%
#'   ggplot() +
#'   geom_point(aes(x = start$x, y = start$y), color = "blue") +
#'   geom_point(aes(x = end$x, y = end$y), color = "red") +
#'   geom_rect(
#'     aes(xmin = network$xmin, xmax = network$xmax, ymin = network$ymin, ymax = network$ymax),
#'     alpha = 0.5, fill = "grey"
#'   ) +
#'   coord_ip(curve = "morton", expand = TRUE) +
#'   theme_ip_light()
#' @seealso
#' `vignette("visualizing-ip-data")` describes the mapping in more detail.
#' @export
coord_ip <- function(canvas_network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16,
                     curve = c("hilbert", "morton"),
                     expand = FALSE) {
  curve <- arg_match(curve)
  curve_order <- as.integer((pixel_prefix - prefix_length(canvas_network)) / 2)
  lim <- as.integer(c(0, 2^curve_order - 1))

  ggplot2::ggproto(NULL, CoordIp,
    canvas_network = canvas_network,
    pixel_prefix = pixel_prefix,
    curve = curve,
    curve_order = curve_order,
    limits = list(x = lim, y = lim),
    expand = expand,
    ratio = 1
  )
}


CoordIp <- ggplot2::ggproto("CoordIp", ggplot2::CoordFixed,

  # CoordIp needs to keep track of some parameters
  # internally as the plot is built. These are stored here.
  params = list(),

  setup_params = function(self, data) {
    list(
      canvas_network = self$canvas_network,
      pixel_prefix = self$pixel_prefix,
      curve = self$curve,
      curve_order = self$curve_order
    )
  },

  setup_data = function(data, params) {
    lapply(data, function(layer_data) {
      for (col in colnames(layer_data)) {

        # ip_address --> ip_address_coords
        if (is_ip_address(layer_data[[col]])) {
          coords <- address_to_cartesian(
            layer_data[[col]], params$canvas_network, params$pixel_prefix, params$curve
          )
          layer_data[[col]] <- ip_address_coords(
            ip = layer_data[[col]],
            x = coords$x, y = coords$y
          )
        }

        # ip_network --> ip_network_coords
        else if (is_ip_network(layer_data[[col]])) {
          coords <- network_to_cartesian(
            layer_data[[col]], params$canvas_network, params$pixel_prefix, params$curve
          )
          layer_data[[col]] <- ip_network_coords(
            ip = layer_data[[col]],
            xmin = coords$xmin, ymin = coords$ymin,
            xmax = coords$xmax, ymax = coords$ymax
          )
        }
      }

      layer_data
    })
  }
)

is_CoordIp <- function(x) inherits(x, "CoordIp")


# ggplot2 scales -----------------------------------------------------------
# these prevent ggplot2 warnings

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_address <- function(x) "identity"

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_network <- function(x) "identity"
