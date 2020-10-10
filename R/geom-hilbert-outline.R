#' Hilbert curve outline
#'
#' Computes and draws the outline of the Hilbert curve used to map IP data to
#' the Cartesian plane. By superimposing this outline on top of a ggip plot,
#' it guides the eye to regions that are close in IP address space.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @section Aesthetics:
#' `geom_curve_outline()` understands the following aesthetics:
#'  - `ip`: An [`ip_network`][`ipaddress::ip_network`] column. By default, the
#'    entire Hilbert curve is shown.
#'  - `curve_order`: How nested is the curve? (default: `3`).
#'  - `closed`: Should the curve outline have closed ends? (default: `FALSE`).
#'  - `alpha`
#'  - `colour`
#'  - `linetype`
#'  - `size`
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The start coordinates for the segment}
#'  \item{xend, yend}{The end coordinates for the segment}
#' }
#'
#' @examples
#' p <- ggplot() + coord_ip() + theme_ip_light()
#'
#' # default shows curve across entire canvas
#' p + geom_hilbert_outline()
#'
#' # only show subnetwork
#' p + geom_hilbert_outline(ip = ip_network("128.0.0.0/2"))
#'
#' # increased nesting
#' p + geom_hilbert_outline(curve_order = 4)
#'
#' # show multiple networks
#' df <- data.frame(
#'   ip = ip_network(c("0.0.0.0/2", "128.0.0.0/4")),
#'   curve_order = c(4, 5),
#'   closed = c(FALSE, TRUE)
#' )
#' p + geom_hilbert_outline(
#'   aes(ip = ip, curve_order = curve_order, closed = closed),
#'   data = df
#' )
#' @export
geom_hilbert_outline <- function(mapping = NULL, data = NULL, ...,
                                 na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  # can use layer without any data
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }

  ggplot2::layer(
    geom = GeomHilbertOutline, data = data, mapping = mapping, stat = "identity",
    position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomHilbertOutline <- ggplot2::ggproto("GeomHilbertOutline", ggplot2::Geom,
  default_aes = ggplot2::aes(
    ip = NULL,
    curve_order = 3,
    closed = FALSE,
    colour = "black",
    size = 0.5,
    linetype = 1,
    alpha = NA
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    if (!is_CoordIp(coord)) {
      stop_missing_coord()
    }
    if (coord$curve != "hilbert") {
      abort('`geom_hilbert_outline()` requires `coord_ip(curve = "hilbert")`.')
    }

    # validate ip aesthetic
    if (is.null(data$ip)) {
      data$ip <- coord$canvas_network
    } else if (is_ip_network_coords(data$ip)) {
      data$ip <- data$ip$ip
    }
    if (!is_ip_network(data$ip)) {
      stop_bad_aes_type("geom_hilbert_outline", "ip", "ip_network")
    }

    segments <- data %>%
      dplyr::distinct() %>%
      dplyr::rowwise(-ip, -curve_order, -closed) %>%
      dplyr::summarize(generate_curve_outline(ip, curve_order, coord, closed)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    ggplot2::GeomSegment$draw_panel(segments, panel_params, coord,
                                    lineend = "round", na.rm = na.rm)
  },

  draw_key = ggplot2::draw_key_path
)

#' Generate a set of line segments that should be drawn to show the outline of
#' the Hilbert curve for a single network.
#'
#' @param network `ip_network` scalar
#' @param curve_order Integer scalar
#' @param coord The `CoordIp` coordinate system.
#' @param closed Logical scalar indicating whether to visualize the sides at
#'   the beginning and end of the path.
#' @return A data.frame with 4 columns: `x`, `y`, `xend`, `yend`.
#'
#' @noRd
generate_curve_outline <- function(network, curve_order, coord, closed) {
  curve_prefix <- (2 * curve_order) + prefix_length(coord$canvas_network)

  if (curve_prefix > prefix_length(network)) {
    network %>%
      subnets(new_prefix = curve_prefix) %>%
      network_to_cartesian(
        canvas_network = coord$canvas_network,
        pixel_prefix = coord$pixel_prefix,
        curve = coord$curve
      ) %>%
      squares_to_sides(closed) %>%
      sides_to_segments(coord)
  } else {
    data.frame(x = double(), y = double(), xend = double(), yend = double())
  }
}

path_direction <- function(x_from, y_from, x_to, y_to) {
  factor(dplyr::case_when(
    x_to > x_from ~ "right",
    x_to < x_from ~ "left",
    y_to > y_from ~ "up",
    y_to < y_from ~ "down",
    TRUE ~ NA_character_
  ), levels = c("right", "left", "up", "down"))
}

translate_endpoints <- function(side, opposite, closed) {
  dplyr::case_when(
    closed ~ side,
    !is.na(side) ~ side,
    opposite == "right" ~ factor("left"),
    opposite == "left" ~ factor("right"),
    opposite == "up" ~ factor("down"),
    opposite == "down" ~ factor("up")
  )
}

snap_to_grid <- function(x, add_offset, limits) {
  dplyr::case_when(
    x %in% limits ~ as.numeric(x),
    add_offset ~ x + 0.5,
    TRUE ~ x - 0.5
  )
}

#' Translate a path of squares into a sequence of square sides to be drawn,
#' in order to visualize the path outline.
#'
#' @param data A data.frame with 4 columns: `xmin`, `ymin`, `xmax`, `ymax`.
#'   There is 1 row per square of the path.
#' @param closed Logical scalar indicating whether to visualize the sides at
#'   the beginning and end of the path.
#' @return A data.frame with 5 columns: `xmin`, `ymin`, `xmax`, `ymax`, `side`.
#'   There is 1 row per side of the path outline.
#'
#' @noRd
squares_to_sides <- function(data, closed) {
  sides_all <- data.frame(side = factor(c("right", "left", "up", "down")))

  sides_not_drawn <- data %>%
    dplyr::mutate(
      xmid = (.data$xmin + .data$xmax) / 2,
      ymid = (.data$ymin + .data$ymax) / 2,
      from = path_direction(.data$xmid, .data$ymid, dplyr::lag(.data$xmid), dplyr::lag(.data$ymid)),
      to = path_direction(.data$xmid, .data$ymid, dplyr::lead(.data$xmid), dplyr::lead(.data$ymid)),
      from = translate_endpoints(.data$from, .data$to, closed),
      to = translate_endpoints(.data$to, .data$from, closed)
    ) %>%
    dplyr::select(-.data$xmid, -.data$ymid)

  # compute drawn sides
  dplyr::full_join(sides_all, sides_not_drawn, by = character()) %>%
    dplyr::filter(
      .data$side != dplyr::coalesce(.data$from, "endcap"),
      .data$side != dplyr::coalesce(.data$to, "endcap")
    ) %>%
    dplyr::select(-.data$from, -.data$to)
}

#' Transforms a sequence of square sides into a sequence of line segments.
#'
#' @param data A data.frame with 5 columns: `xmin`, `ymin`, `xmax`, `ymax`, `side`.
#'   There is 1 row per side of the path outline.
#' @param coord The `CoordIp` coordinate system.
#' @return A data.frame with 4 columns: `x`, `y`, `xend`, `yend`.
#'   There is 1 row per side of the path outline.
#'
#' @noRd
sides_to_segments <- function(data, coord) {
  data %>%
    dplyr::mutate(
      x = dplyr::if_else(.data$side == "right", .data$xmax, .data$xmin),
      y = dplyr::if_else(.data$side == "up", .data$ymax, .data$ymin),
      xend = dplyr::if_else(.data$side == "left", .data$xmin, .data$xmax),
      yend = dplyr::if_else(.data$side == "down", .data$ymin, .data$ymax),

      x = snap_to_grid(.data$x, .data$side == "right", coord$limits$x),
      y = snap_to_grid(.data$y, .data$side == "up", coord$limits$y),
      xend = snap_to_grid(.data$xend, .data$side != "left", coord$limits$x),
      yend = snap_to_grid(.data$yend, .data$side != "down", coord$limits$y)
    ) %>%
    dplyr::select(.data$x, .data$y, .data$xend, .data$yend)
}


#--- copied from ggplot2

# Convenience function used by `stat_function()` and
# `geom_function()` to convert empty input data into
# non-empty input data without touching any non-empty
# input data that may have been provided.
ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    data.frame(group = 1)
  } else {
    data
  }
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.waive <- function(x) inherits(x, "waiver")
