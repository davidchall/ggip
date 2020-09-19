#' Map IP data to Cartesian coordinates
#'
#' These functions are used internally by [coord_ip()] to map
#' [`ip_address`][`ipaddress::ip_address`] and [`ip_network`][`ipaddress::ip_network`]
#' vectors to Cartesian coordinates. They are exposed externally to support use
#' of these coordinates outside of ggplot2.
#'
#' @param address An [`ip_address`][`ipaddress::ip_address`] vector
#' @param network An [`ip_network`][`ipaddress::ip_network`] vector
#' @param canvas_network An [`ip_network`][`ipaddress::ip_network`] scalar that
#'   determines the region of IP space visualized by the entire 2D grid. The
#'   default shows the entire IPv4 address space.
#' @param pixel_prefix An integer scalar that sets the prefix length of the
#'   network represented by a single pixel. The default value is 16. Increasing
#'   this effectively improves the resolution of the plot.
#' @param curve A string to choose the space-filling curve. Choices are
#'   `"hilbert"` (default) and `"morton"`.
#' @return A data.frame containing columns:
#'  * `address_to_cartesian()`: `x` and `y`
#'  * `network_to_cartesian()`: `xmin`, `ymin`, `xmax` and `ymax`
#'
#' @examples
#' address_to_cartesian(ip_address("192.168.0.1"))
#'
#' network_to_cartesian(ip_network("224.0.0.0/4"))
#' @name ip_to_cartesian
NULL

#' @rdname ip_to_cartesian
#' @export
address_to_cartesian <- function(address,
                                 canvas_network = ip_network("0.0.0.0/0"),
                                 pixel_prefix = 16,
                                 curve = c("hilbert", "morton")) {
  if (!is_ip_address(address)) {
    abort("`address` must be a vector with type <ip_address>.")
  }
  validate_mapping_params(canvas_network, pixel_prefix)
  curve <- arg_match(curve)

  wrap_address_to_cartesian(address, canvas_network, pixel_prefix, curve)
}

#' @rdname ip_to_cartesian
#' @export
network_to_cartesian <- function(network,
                                 canvas_network = ip_network("0.0.0.0/0"),
                                 pixel_prefix = 16,
                                 curve = c("hilbert", "morton")) {
  if (!is_ip_network(network)) {
    abort("`network` must be a vector with type <ip_network>.")
  }
  validate_mapping_params(canvas_network, pixel_prefix)
  curve <- arg_match(curve)

  wrap_network_to_cartesian(network, canvas_network, pixel_prefix, curve)
}


validate_mapping_params <- function(canvas_network, pixel_prefix) {
  if (!(is_ip_network(canvas_network) && length(canvas_network) == 1)) {
    abort("`canvas_network` must be a scalar with type <ip_network>.")
  }
  if (is.na(canvas_network)) {
    abort("`canvas_network` cannot be NA.")
  }

  if (!is_scalar_integerish(pixel_prefix) || pixel_prefix < 0 || is.na(pixel_prefix)) {
    abort("`pixel_prefix` must be a positive integer scalar.")
  }

  if (pixel_prefix < 0 || pixel_prefix > max_prefix_length(canvas_network)) {
    space <- ifelse(is_ipv6(canvas_network), "IPv6", "IPv4")
    msg <- glue::glue("Pixel prefix length must not be greater than {max_prefix_length(canvas_network)}.")
    bullets <- format_error_bullets(c(
      "i" = glue::glue("Canvas uses {space} address space."),
      "x" = glue::glue("Pixel has prefix length {pixel_prefix}.")
    ))
    abort(msg, body = bullets)
  }

  n_bits <- pixel_prefix - prefix_length(canvas_network)
  if (n_bits < 0) {
    msg <- "Pixel prefix length must be greater than canvas."
    bullets <- format_error_bullets(c(
      "x" = glue::glue("Canvas has prefix length {prefix_length(canvas_network)}."),
      "x" = glue::glue("Pixel has prefix length {pixel_prefix}.")
    ))
    abort(msg, body = bullets)
  }

  if (n_bits %% 2 != 0) {
    msg <- "The difference between canvas and pixel prefix lengths must be even."
    bullets <- format_error_bullets(c(
      "x" = glue::glue("Canvas has prefix length {prefix_length(canvas_network)}."),
      "x" = glue::glue("Pixel has prefix length {pixel_prefix}.")
    ))
    abort(msg, body = bullets)
  }

  # enforce a sensible maximum resolution (16.7 million pixels)
  if (n_bits > 24) {
    n_pixels <- format(2^(n_bits / 2), big.mark = ",")
    msg <- "The difference between canvas and pixel prefix lengths must not be greater than 24."
    bullets <- format_error_bullets(c(
      "x" = glue::glue("Canvas has prefix length {prefix_length(canvas_network)}."),
      "x" = glue::glue("Pixel has prefix length {pixel_prefix}."),
      "i" = glue::glue("These values would produce a plot with {n_pixels} x {n_pixels} pixels.")
    ))
    abort(msg, body = bullets)
  }
}
