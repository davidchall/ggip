#' Heatmap of IP data
#'
#' @section Aesthetics:
#'  - `ip`: IP data ([`ipaddress::ip_address`])
#'  - `z`: Value passed to the summary function (only required if `fun != "count"`).
#' @section Computed variables:
#'  - `x`, `y`: Cartesian coordinates of encoded IP data
#'  - `value`: Value of summary statistic
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param canvas_network An [`ipaddress::ip_network`] scalar
#' @param pixel_prefix An integer scalar
#' @param curve A string indicating the space-filling curve used to map IP data
#'   to the Cartesian plane. Choices are `"hilbert"` (default) and `"morton"`.
#' @param fun Summary function
#' @param fun.args A list of extra arguments to pass to `fun`
#' @param drop if `TRUE` removes all cells with 0 counts.
#' @export
stat_ip_heatmap <- function(mapping = NULL, data = NULL, geom = "raster",
                            position = "identity", ...,
                            canvas_network = NULL, pixel_prefix = NULL,
                            curve = c("hilbert", "morton"),
                            fun = "count", fun.args = list(), drop = TRUE,
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatIpHeatmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      canvas_network = canvas_network,
      pixel_prefix = pixel_prefix,
      curve = curve,
      fun = fun,
      fun.args = fun.args,
      drop = drop,
      ...
    )
  )
}

StatIpHeatmap <- ggplot2::ggproto("StatIpHeatmap", ggplot2::Stat,
  required_aes = "ip",

  default_aes = ggplot2::aes(z = NULL, fill = ggplot2::after_stat(value)),

  extra_params = c(
    "na.rm",
    "fun", "fun.args", "drop",
    "canvas_network", "pixel_prefix", "curve"
  ),

  setup_params = function(data, params) {
    params <- default_coord_params(data, params)
    params
  },

  setup_data = function(data, params) {
    # use 'ip' column to populate 'x' and 'y' columns with grid values
    address_to_pixel(data, params$canvas_network, params$pixel_prefix, params$curve)
  },

  compute_group = function(data, scales,
                           fun = "count", fun.args = list(), drop = TRUE,
                           curve_order,
                           ...) {
    if (is_formula(fun)) {
      fun <- as_function(fun)
    }

    out <- if (is_scalar_character(fun) && fun == "count") {
      tapply_df(data$x, list(x = data$x, y = data$y), length, drop = drop)
    } else {
      f <- function(x) do.call(fun, c(list(quote(x)), fun.args))
      tapply_df(data$z, list(x = data$x, y = data$y), f, drop = drop)
    }

    tidyr::complete(
      out,
      tidyr::expand(out, x = 1:2^curve_order, y = 1:2^curve_order),
      fill = list(value = 0)
    )
  }
)

# Copied from ggplot2
ulevels <- function(x) {
  if (is.factor(x)) {
    x <- addNA(x, TRUE)
    factor(levels(x), levels(x), exclude = NULL)
  } else {
    sort(unique(x))
  }
}

# Adaptation of tapply that returns a data frame instead of a matrix
tapply_df <- function(x, index, fun, ..., drop = TRUE) {
  labels <- lapply(index, ulevels)
  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  grps <- split(x, index)
  names(grps) <- NULL
  out$value <- unlist(lapply(grps, fun, ...))

  if (drop) {
    n <- vapply(grps, length, integer(1))
    out <- out[n > 0, , drop = FALSE]
  }

  out
}
