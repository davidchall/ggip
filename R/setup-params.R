#' Set default values for parameters determining the coordinate system of
#' ggip plots. That is:
#'  * `canvas_network`
#'  * `pixel_prefix`
#'  * `curve`
#'
#' It also sets a `curve_order` helper parameter.
#'
#' @param data Data frame containing `ip` variable
#' @param params A named list of parameters
#' @return A named list of parameters (with defaults set)
#' @noRd
default_coord_params <- function(data, params) {
  # default: plot entire address space
  # default: show IPv6 space only if all addresses are IPv6
  if (is.null(params$canvas_network)) {
    if (all(is_ipv6(data$ip))) {
      params$canvas_network <- ip_network("::/0")
    } else {
      params$canvas_network <- ip_network("0.0.0.0/0")
    }
  }

  if (!(is_ip_network(params$canvas_network) && length(params$canvas_network) == 1)) {
    abort("'canvas_network' must be an ip_network scalar")
  }

  # default: display 16 bits
  if (is.null(params$pixel_prefix)) {
    default_curve_order <- 8L
    params$pixel_prefix <- min(
      prefix_length(params$canvas_network) + (2 * default_curve_order),
      max_prefix_length(params$canvas_network)
    )
  }

  # helper parameter
  params$curve_order <- as.integer((params$pixel_prefix - prefix_length(params$canvas_network)) / 2)

  params
}
