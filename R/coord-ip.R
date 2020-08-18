#' Coordinate system for IP data
#'
#' @description
#' A ggplot2 coordinate system that maps a range of address space onto a
#' two-dimensional grid using a space-filling curve.
#'
#' `coord_ip()` translates all [`ip_address`][`ipaddress::ip_address`] and
#' [`ip_network`][`ipaddress::ip_network`] vectors to Cartesian coordinates,
#' ready for use by ggplot2 layers (see Details). This ensures all layers use a
#' common mapping.
#'
#' @details
#' `coord_ip()` uses nested dataframes to store the result of the mapping. This
#' means each [`ip_address`][`ipaddress::ip_address`] or
#' [`ip_network`][`ipaddress::ip_network`] column in your original dataset is
#' converted to a dataframe column. When specifying ggplot2 aesthetics, you'll
#' need to use `$` to access the nested data (see Examples).
#'
#' Each [`ip_address`][`ipaddress::ip_address`] column will be replaced with a
#' dataframe containing the following columns:
#'
#' | Column name | Data type    | Description        |
#' |:------------|:-------------|:-------------------|
#' | `ip`        | `ip_address` | Original IP data   |
#' | `x`         | `integer`    | Pixel x coordinate |
#' | `y`         | `integer`    | Pixel y coordinate |
#'
#' Each [`ip_network`][`ipaddress::ip_network`] column will be replaced with a
#' dataframe containing the following columns:
#'
#' | Column name | Data type    | Description                  |
#' |:------------|:-------------|:-----------------------------|
#' | `ip`        | `ip_network` | Original IP data             |
#' | `xmin`      | `integer`    | Bounding box xmin coordinate |
#' | `ymin`      | `integer`    | Bounding box ymin coordinate |
#' | `xmax`      | `integer`    | Bounding box xmax coordinate |
#' | `ymax`      | `integer`    | Bounding box ymax coordinate |
#'
#' @inheritParams ip_to_cartesian
#' @inheritParams ggplot2::coord_fixed
#' @examples
#' options(tidyverse.quiet = TRUE)
#' library(tidyverse)
#' library(ipaddress)
#'
#' tibble(address = ip_address(c("0.0.0.0", "128.0.0.0", "192.168.0.1"))) %>%
#'   ggplot(aes(x = address$x, y = address$y, label = address$ip)) +
#'   geom_point() +
#'   geom_label(nudge_x = c(10, 0, -10), nudge_y = -10) +
#'   coord_ip() +
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
#'   coord_ip(curve = "morton") +
#'   theme_ip_light()
#' @seealso
#' `vignette("visualizing-ip-data")` describes the mapping in more detail.
#' @export
coord_ip <- function(canvas_network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16,
                     curve = c("hilbert", "morton"),
                     expand = TRUE) {

  curve_order <- as.integer((pixel_prefix - prefix_length(canvas_network)) / 2)
  lim <- as.integer(c(0, 2 ^ curve_order - 1))

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
      # replace ip_address and ip_network columns with dataframe columns
      for (col in colnames(layer_data)) {
        if (is_ip_address(layer_data[[col]])) {
          # dataframe output has columns: ip, x, y
          layer_data[[col]] <- cbind(
            data.frame(ip = layer_data[[col]]),
            address_to_cartesian(layer_data[[col]], params$canvas_network, params$pixel_prefix, params$curve)
          )
        } else if (is_ip_network(layer_data[[col]])) {
          # dataframe output has columns: ip, xmin, ymin, xmax, ymax
          layer_data[[col]] <- cbind(
            data.frame(ip = layer_data[[col]]),
            network_to_cartesian(layer_data[[col]], params$canvas_network, params$pixel_prefix, params$curve)
          )
        }
      }
      layer_data
    })
  }
)

is_CoordIp <- function(x) inherits(x, "CoordIp")


# ggplot2 scales -----------------------------------------------------------

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_address <- function(x) "identity"

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_network <- function(x) "identity"
