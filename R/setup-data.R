validate_plot_params <- function(canvas_network, pixel_prefix, curve) {
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

address_to_pixel <- function(data, canvas_network, pixel_prefix, curve) {
  if (!is_ip_address(data$ip)) {
    abort("'ip' aesthetic must be an ip_address vector")
  }

  validate_plot_params(canvas_network, pixel_prefix, curve)

  cbind(data, address_to_cartesian(data$ip, canvas_network, pixel_prefix))
}


# ggplot2 scales -----------------------------------------------------------

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_address <- function(x) "identity"

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_network <- function(x) "identity"
