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
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param fun Summary function
#' @param fun.args A list of extra arguments to pass to `fun`
#' @param drop if `TRUE` removes all cells with 0 counts.
#' @export
stat_ip_heatmap <- function(mapping = NULL, data = NULL, geom = "raster",
                            position = "identity", ...,
                            fun = "count", fun.args = list(), drop = TRUE,
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatIpHeatmap, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      fun.args = fun.args,
      drop = drop,
      ...
    )
  )
}

StatIpHeatmap <- ggplot2::ggproto("StatIpHeatmap", ggplot2::Stat,
  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(z = NULL, fill = ggplot2::after_stat(value)),

  extra_params = c(
    "na.rm",
    "fun", "fun.args", "drop"
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
                           fun = "count", fun.args = list(), drop = TRUE,
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

    # fill missing pixels so raster works
    x_range <- seq(coord$limits$x[1], coord$limits$x[2])
    y_range <- seq(coord$limits$y[1], coord$limits$y[2])
    tidyr::complete(out, tidyr::expand(out, x = x_range, y = y_range))
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
