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
#' @inheritParams ggplot2::coord_fixed
#' @export
coord_ip <- function(network = ip_network("0.0.0.0/0"),
                     pixel_prefix = 16,
                     curve = c("hilbert", "morton"),
                     expand = FALSE) {

  validate_coord_params(network, pixel_prefix, curve)

  curve_order <- as.integer((pixel_prefix - prefix_length(network)) / 2)
  lim <- as.integer(c(0, 2 ^ curve_order - 1))

  ggplot2::ggproto(NULL, CoordIp,
    network = network,
    pixel_prefix = pixel_prefix,
    curve = curve,
    curve_order = curve_order,
    limits = list(x = lim, y = lim),
    expand = expand
  )
}


validate_coord_params <- function(network, pixel_prefix, curve) {
  if (!(is_ip_network(network) && length(network) == 1)) {
    abort("`network` must be an ip_network scalar")
  }

  if (!is_scalar_integerish(pixel_prefix) || pixel_prefix < 0) {
    abort("`pixel_prefix` must be a positive integer scalar")
  }

  if (pixel_prefix < 0 || pixel_prefix > max_prefix_length(network)) {
    abort(sprintf(
      "`pixel_prefix` cannot be greater than %i for %s",
      max_prefix_length(network),
      ifelse(is_ipv6(network), "IPv6", "IPv4")
    ))
  }

  n_bits <- pixel_prefix - prefix_length(network)
  if (n_bits < 0) {
    abort("`pixel_prefix` must be greater than prefix_length(`network`)")
  }

  if (n_bits %% 2 != 0) {
    abort("The difference between prefix_length(`network`) and `pixel_prefix` cannot be odd")
  }

  if (n_bits > 24) {
    n_pixels <- 2 ^ (n_bits / 2)
    abort(paste0(
      "The difference between prefix_length(`network`) and `pixel_prefix` is too big.",
      "\n",
      "Current parameters would result in plot with ", n_pixels, "x", n_pixels, " pixels"
    ))
  }

  curve <- arg_match(curve, c("hilbert", "morton"))
}


CoordIp <- ggplot2::ggproto("CoordIp", ggplot2::CoordFixed,

  # CoordIp needs to keep track of some parameters
  # internally as the plot is built. These are stored here.
  params = list(),

  setup_params = function(self, data) {
    list(
      network = self$network,
      pixel_prefix = self$pixel_prefix,
      curve = self$curve,
      curve_order = self$curve_order
    )
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
