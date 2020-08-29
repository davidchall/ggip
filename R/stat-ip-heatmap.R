#' Heatmap of IP data
#'
#' @section Aesthetics:
#' `stat_ip_heatmap()` understands the following aesthetics (required aesthetics
#' are in bold):
#'  - **`x`**
#'  - **`y`**
#'  - `z`: Value passed to the summary function (required if `fun` is used)
#'  - `ip`: [`ip_address`][`ipaddress::ip_address`] vector
#' @section Computed variables:
#'  - `value`: Value of summary statistic (or `count` if `fun` not used)
#'  - `count`: Number of observations
#'  - `address_count`: Number of unique addresses
#'  - `address_proportion`: Observed proportion of total addresses
#' @section Summary function:
#' The `data` might contain multiple rows per pixel of the heatmap, so we use
#' a summary function to reduce this information to a single value to display.
#' This function receives the `data` column specified by the `z` aesthetic
#' and will also receive arguments specified by `fun.args`.
#'
#' The `fun` argument can be specified in multiple ways:
#' \describe{
#' \item{`NULL`}{If no summary function is provided, the number of observations
#'   is computed. In this case, you don't need to specify the `z` aesthetic,
#'   and the computed variables `value` and `count` will be equal.}
#' \item{String}{The name of an existing function (e.g. `fun = "mean"`).}
#' \item{Function}{Either provide an existing function (e.g. `fun = mean`) or
#'   define a new function (e.g. `fun = function(x) sum(x^2)`).}
#' \item{Formula}{A function can also be created from a formula. This uses `.x`
#'   as the summarized variable (e.g. `fun = ~ sum(.x^2)`).}
#' }
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param fun Summary function (see section below for details). If `NULL` (the
#'   default), the number of observations is computed.
#' @param fun.args A list of extra arguments to pass to `fun`
#' @export
stat_ip_heatmap <- function(mapping = NULL, data = NULL, geom = "raster",
                            position = "identity", ...,
                            fun = NULL, fun.args = list(),
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

  default_aes = ggplot2::aes(
    z = NULL,
    ip = NULL,
    fill = ggplot2::after_stat(value)
    ),

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
                           fun = NULL, fun.args = list(), ...) {
    compute_ip_heatmap(data, coord = coord, fun = fun, fun.args = fun.args)
  }
)

compute_ip_heatmap <- function(data, coord, fun, fun.args) {
  # support formula interface
  if (is_formula(fun)) {
    fun <- as_function(fun)
  }

  summarize_count <- is.null(fun)
  if (!summarize_count && !("z" %in% colnames(data))) {
    abort("stat_ip_heatmap() requires `z` aesthetic when using non-default summary function")
  }

  # summarize grid found in data
  index <- list(x = data$x, y = data$y)
  labels <- lapply(index, function(x) sort(unique(x)))
  out <- expand.grid(labels, KEEP.OUT.ATTRS = FALSE)

  out$count <- summarize_grid(data$x, index, length)
  out$value <- if (summarize_count) {
    out$count
  } else {
    f <- function(x) do.call(fun, c(list(quote(x)), fun.args))
    summarize_grid(data$z, index, f)
  }
  if ("ip" %in% colnames(data)) {
    f <- function(x) length(unique(x))
    out$address_count <- summarize_grid(data$ip, index, f)

    bits_per_pixel <- max_prefix_length(coord$canvas_network) - coord$pixel_prefix
    out$address_proportion <- out$address_count / (2^bits_per_pixel)
  }

  # fill remaining grid so raster works
  range <- coord$limits$x[1]:coord$limits$x[2]
  fill_na <- list(count = 0, proportion = 0)
  if (summarize_count) {
    fill_na$value <- 0
  }
  tidyr::complete(out, tidyr::expand(out, x = range, y = range), fill = fill_na)
}

summarize_grid <- function(x, index, fun) {
  grps <- split(x, index)
  names(grps) <- NULL
  unlist(lapply(grps, fun))
}
