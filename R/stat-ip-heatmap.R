#' Heatmap of IP data
#'
#' @section Aesthetics:
#' `stat_ip_heatmap()` understands the following aesthetics (required aesthetics
#' are in bold):
#'  - **`x`**
#'  - **`y`**
#'  - `z`: Value passed to the summary function (only required if `fun != "count"`).
#' @section Computed variables:
#'  - `value`: Value of summary statistic
#'  - `count`
#'  - `proportion`
#' @section Summary function:
#'
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param fun Summary function
#' @param fun.args A list of extra arguments to pass to `fun`
#' @export
stat_ip_heatmap <- function(mapping = NULL, data = NULL, geom = "raster",
                            position = "identity", ...,
                            fun = "count", fun.args = list(),
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatIpHeatmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      fun.args = fun.args,
      ...
    )
  )
}

StatIpHeatmap <- ggplot2::ggproto("StatIpHeatmap", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(z = NULL, fill = ggplot2::after_stat(value)),

  extra_params = c(
    "na.rm",
    "fun", "fun.args"
  ),

  compute_layer = function(self, data, params, layout) {
    if (!is_CoordIp(layout$coord)) {
      abort("Must call coord_ip() when using ggip")
    }

    # add coord to the params, so it can be forwarded to compute_group()
    params$coord <- layout$coord
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord,
                           fun = "count", fun.args = list(), ...) {
    summarize_grid(data, coord = coord, fun = fun, fun.args = fun.args)
  }
)

summarize_grid <- function(data, coord, fun, fun.args) {
  # support formula interface
  if (is_formula(fun)) {
    fun <- as_function(fun)
  }

  summarize_count <- is_scalar_character(fun) && fun == "count"
  if (!summarize_count && !("z" %in% colnames(data))) {
    abort("stat_ip_heatmap() requires `z` aesthetic when using non-default summary function")
  }

  # summarize grid found in data
  index <- list(x = data$x, y = data$y)
  labels <- lapply(index, function(x) sort(unique(x)))
  grps <- if (summarize_count) {
    split(data$x, index)
  } else {
    split(data$z, index)
  }
  names(grps) <- NULL

  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE)
  out$count <- vapply(grps, length, integer(1))
  bits_per_pixel <- max_prefix_length(coord$canvas_network) - coord$pixel_prefix
  out$proportion <- out$count / (2^bits_per_pixel)
  out$value <- if (summarize_count) {
    out$count
  } else {
    f <- function(x) do.call(fun, c(list(quote(x)), fun.args))
    unlist(lapply(grps, f))
  }

  # fill remaining grid so raster works
  range <- coord$limits$x[1]:coord$limits$x[2]
  fill_na <- list(count = 0, proportion = 0)
  if (summarize_count) {
    fill_na$value <- 0
  }
  tidyr::complete(out, tidyr::expand(out, x = range, y = range), fill = fill_na)
}
