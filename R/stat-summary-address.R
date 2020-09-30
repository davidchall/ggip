#' Summarize IP addresses on a heatmap
#'
#' Addresses are grouped into networks determined by the `pixel_prefix` argument
#' of `coord_ip()`. Then the `z` values are summarized with summary function `fun`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param fun Summary function (see section below for details). If `NULL` (the
#'   default), the observations are simply counted.
#' @param fun.args A list of extra arguments to pass to `fun`.
#'
#' @section Aesthetics:
#' `stat_summary_address()` understands the following aesthetics (required
#' aesthetics are in bold):
#'  - **`ip`**: An [`ip_address`][`ipaddress::ip_address`] column
#'  - `z`: Value passed to the summary function (required if `fun` is used)
#'  - `fill`: Default is `after_stat(value)`
#'  - `alpha`
#'
#' @section Computed variables:
#' The following variables are available to [`after_stat()`][ggplot2::after_stat()]:
#'  - `value`: Value of summary statistic
#'  - `count`: Number of observations
#'
#' @section Summary function:
#' The `data` might contain multiple rows per pixel of the heatmap, so a summary
#' function reduces this information to a single value to display.
#' This function receives the `data` column specified by the `z` aesthetic
#' and also receives arguments specified by `fun.args`.
#'
#' The `fun` argument can be specified in multiple ways:
#' \describe{
#' \item{`NULL`}{If no summary function is provided, the number of observations
#'   is computed. In this case, you don't need to specify the `z` aesthetic,
#'   and the computed variables `value` and `count` will be equal.}
#' \item{string}{The name of an existing function (e.g. `fun = "mean"`).}
#' \item{function}{Either provide an existing function (e.g. `fun = mean`) or
#'   define a new function (e.g. `fun = function(x) sum(x^2)`).}
#' \item{formula}{A function can also be created from a formula. This uses `.x`
#'   as the summarized variable (e.g. `fun = ~ sum(.x^2)`).}
#' }
#'
#' @examples
#' dat <- data.frame(
#'   ip = sample_ipv4(10000),
#'   weight = runif(10000)
#' )
#'
#' p <- ggplot(dat, aes(ip = ip)) +
#'   coord_ip() +
#'   theme_ip_light()
#'
#' # simple count of observations
#' p +
#'   stat_summary_address() +
#'   scale_fill_viridis_c(trans = "log2", na.value = "black", guide = "none")
#'
#' # compute mean weight
#' p +
#'   stat_summary_address(aes(z = weight), fun = ~ mean(.x)) +
#'   scale_fill_viridis_c(na.value = "black", guide = "none")
#' @export
stat_summary_address <- function(mapping = NULL, data = NULL, ...,
                                 fun = NULL, fun.args = list(),
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatSummaryAddress, data = data, mapping = mapping, geom = "raster",
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      fun.args = fun.args,
      ...
    )
  )
}

StatSummaryAddress <- ggplot2::ggproto("StatSummaryAddress", ggplot2::Stat,

  # The `ip` aesthetic is required, but putting it in required_aes causes a very
  # slow check for missing values. It's much faster to simply check `x` and `y`
  # for missing values. These are always NA when `ip` is NA.
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    ip = NULL,
    z = NULL,
    fill = ggplot2::after_stat(value)
  ),

  extra_params = c("na.rm", "fun", "fun.args"),

  compute_layer = function(self, data, params, layout) {
    if (!is_CoordIp(layout$coord)) {
      stop_missing_coord()
    }

    # validate ip aesthetic
    if (is.null(data$ip)) {
      stop_missing_aes("stat_summary_address", "ip")
    } else if (is_ip_address_coords(data$ip)) {
      data$x <- data$ip$x
      data$y <- data$ip$y
      data$ip <- data$ip$ip
    } else if (is_ip_address(data$ip)) {
      abort("The `ip` aesthetic of `stat_summary_address()` must map to a `data` variable.")
    } else {
      stop_bad_aes_type("stat_summary_address", "ip", "ip_address")
    }

    if (!is.null(params$fun) && !("z" %in% colnames(data))) {
      abort("`stat_summary_address()` must have `z` aesthetic when using `fun` argument.")
    }

    # add coord to the params, so it reaches compute_group()
    params$coord <- layout$coord
    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord,
                           fun = NULL, fun.args = list(), ...) {
    summarize_addresses(data, scales, coord, fun, fun.args)
  }
)

summarize_addresses <- function(data, scales, coord, fun, fun.args) {
  summarize_grid <- function(x, index, fun) {
    grps <- split(x, index)
    names(grps) <- NULL
    unlist(lapply(grps, fun))
  }

  # support formula interface
  if (is_formula(fun)) {
    fun <- as_function(fun)
  }

  summarize_count <- is.null(fun)

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

  # fill remaining grid so raster works
  range <- coord$limits$x[1]:coord$limits$x[2]
  fill_na <- list(count = 0)
  if (summarize_count) {
    fill_na$value <- 0
  }
  tidyr::complete(out, tidyr::expand(out, x = range, y = range), fill = fill_na)
}
