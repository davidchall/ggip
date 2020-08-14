#' Map IP data onto Cartesian coordinates
#'
#' A coordinate system that maps a range of address space onto a two-dimensional
#' grid using a space-filling curve.
#'
#' @param network An [`ipaddress::ip_network`] scalar that determines the
#'   network shown by the plot. The default shows the entire IPv4 address space.
#' @param pixel_prefix An integer scalar that determines the number of addresses
#'   represented by a single pixel. It sets the prefix length of the
#'   corresponding network. The default value is 16.
#' @param curve A string to choose the space-filling curve. Choices are
#'   `"hilbert"` (default) and `"morton"`.
#' @export
coord_ip <- function(network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16,
                     curve = c("hilbert", "morton")) {
  ggplot2::ggproto(NULL, CoordIp,
    network = network,
    pixel_prefix = pixel_prefix,
    curve = curve,
    expand = FALSE
  )
}


validate_coord_params <- function(network, pixel_prefix, curve) {
  if (!(is_ip_network(network) && length(network) == 1)) {
    abort("'network' must be an ip_network scalar")
  }

  if (!is_scalar_integerish(pixel_prefix)) {
    abort("'pixel_prefix' must be an integer scalar")
  }

  if (pixel_prefix < 0 || pixel_prefix > max_prefix_length(network)) {
    version <- ifelse(is_ipv6(network), "IPv6", "IPv4")
    abort(sprintf("'pixel_prefix' is invalid for %s", version))
  }

  n_bits <- pixel_prefix - prefix_length(network)
  if (n_bits < 0) {
    abort("'pixel_prefix' must be greater than 'network'")
  }

  if (n_bits %% 2 != 0) {
    abort("Attempted to display odd number of bits")
  }

  if (n_bits > 24) {
    abort("Too much data (shrink 'network' or reduce 'pixel_prefix')")
  }

  curve <- arg_match(curve, c("hilbert", "morton"))
}


CoordIp <- ggplot2::ggproto("CoordIp", ggplot2::CoordFixed,

  # CoordIp needs to keep track of some parameters
  # internally as the plot is built. These are stored here.
  params = list(),

  get_curve_order = function(self) {
    as.integer((self$pixel_prefix - prefix_length(self$network)) / 2)
  },

  setup_params = function(self, data) {
    validate_coord_params(self$network, self$pixel_prefix, self$curve)

    list(
      network = self$network,
      pixel_prefix = self$pixel_prefix,
      curve = self$curve
    )
  }
)


# ggplot2 scales -----------------------------------------------------------

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_address <- function(x) "identity"

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_network <- function(x) "identity"
