#' Coordinate system for IP data
#'
#' A coordinate system that maps a range of address space onto a two-dimensional
#' grid using a space-filling curve.
#'
#' @inheritParams ip_to_cartesian
#' @inheritParams ggplot2::coord_fixed
#' @export
coord_ip <- function(canvas_network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16,
                     curve = c("hilbert", "morton"),
                     expand = FALSE) {

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
