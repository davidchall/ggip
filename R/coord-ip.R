#' Map IP data onto Cartesian coordinate system
#'
#' @param network An [`ipaddress::ip_network`] scalar
#' @param pixel_prefix An integer scalar
#' @param curve A string indicating the space-filling curve used to map IP data
#'   to the Cartesian plane. Choices are `"hilbert"` (default) and `"morton"`.
#' @export
coord_ip <- function(network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16L,
                     curve = c("hilbert", "morton")) {
  ggplot2::ggproto(NULL, CoordIp,
    network = network,
    pixel_prefix = pixel_prefix,
    curve = curve,
    expand = TRUE
  )
}

validate_coord_params <- function(canvas_network, pixel_prefix, curve) {
  if (!(is_ip_network(canvas_network) && length(canvas_network) == 1)) {
    abort("'canvas_network' must be an ip_network scalar")
  }

  if (!is_scalar_integerish(pixel_prefix)) {
    abort("'pixel_prefix' must be an integer scalar")
  }

  if (pixel_prefix < 0 || pixel_prefix > max_prefix_length(canvas_network)) {
    version <- ifelse(is_ipv6(canvas_network), "IPv6", "IPv4")
    abort(sprintf("'pixel_prefix' is invalid for %s", version))
  }

  n_bits <- pixel_prefix - prefix_length(canvas_network)
  if (n_bits %% 2 != 0) {
    abort("Attempted to display odd number of bits")
  }

  if (n_bits > 24) {
    abort("Too much data (shrink 'canvas_network' or reduce 'pixel_prefix')")
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
