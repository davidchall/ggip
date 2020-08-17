#' Map IP data to Cartesian coordinates
#'
#' @param address An [`ipaddress::ip_address`] vector
#' @param network An [`ipaddress::ip_network`] vector
#' @param canvas_network An [`ipaddress::ip_network`] scalar that determines the
#'   network visualized by the plotted canvas. The default shows the entire IPv4
#'   address space.
#' @param pixel_prefix An integer scalar that determines the number of addresses
#'   represented by a single pixel. It sets the prefix length of the
#'   corresponding network. The default value is 16.
#' @param curve A string to choose the space-filling curve. Choices are
#'   `"hilbert"` (default) and `"morton"`.
#' @name ip_to_cartesian
NULL

#' @rdname ip_to_cartesian
#' @export
address_to_cartesian <- function(address,
                                 canvas_network = ip_network("0.0.0.0/0"),
                                 pixel_prefix = 16,
                                 curve = "hilbert") {
  if (!is_ip_address(address)) {
    abort("'address' must be an ip_address vector")
  }
  validate_mapping_params(canvas_network, pixel_prefix, curve)

  wrap_address_to_cartesian(address, canvas_network, pixel_prefix, curve)
}

#' @rdname ip_to_cartesian
#' @export
network_to_cartesian <- function(network,
                                 canvas_network = ip_network("0.0.0.0/0"),
                                 pixel_prefix = 16,
                                 curve = "hilbert") {
  if (!is_ip_network(network)) {
    abort("'network' must be an ip_address vector")
  }
  validate_mapping_params(canvas_network, pixel_prefix, curve)

  wrap_network_to_cartesian(network, canvas_network, pixel_prefix, curve)
}


validate_mapping_params <- function(canvas_network, pixel_prefix, curve) {
  if (!(is_ip_network(canvas_network) && length(canvas_network) == 1)) {
    abort("`canvas_network` must be an ip_network scalar")
  }
  if (is.na(canvas_network)) {
    abort("`canvas_network` cannot be NA")
  }

  if (!is_scalar_integerish(pixel_prefix) || pixel_prefix < 0 || is.na(pixel_prefix)) {
    abort("`pixel_prefix` must be a positive integer scalar")
  }

  if (pixel_prefix < 0 || pixel_prefix > max_prefix_length(canvas_network)) {
    abort(sprintf(
      "`pixel_prefix` cannot be greater than %i for %s",
      max_prefix_length(canvas_network),
      ifelse(is_ipv6(canvas_network), "IPv6", "IPv4")
    ))
  }

  n_bits <- pixel_prefix - prefix_length(canvas_network)
  if (n_bits < 0) {
    abort("`pixel_prefix` must be greater than prefix_length(`canvas_network`)")
  }

  if (n_bits %% 2 != 0) {
    abort("The difference between prefix_length(`canvas_network`) and `pixel_prefix` cannot be odd")
  }

  if (n_bits > 24) {
    n_pixels <- 2 ^ (n_bits / 2)
    abort(paste0(
      "The difference between prefix_length(`canvas_network`) and `pixel_prefix` is too big.",
      "\n",
      "Current parameters would result in plot with ", n_pixels, "x", n_pixels, " pixels"
    ))
  }

  curve <- arg_match(curve, c("hilbert", "morton"))
}
