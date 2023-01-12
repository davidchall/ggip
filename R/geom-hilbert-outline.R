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
#'  - `linewidth`
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
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {

    if (!is_CoordIp(coord)) {
      cli::cli_abort("{.pkg ggip} plots require {.fn coord_ip}.")
    }
    if (coord$curve != "hilbert") {
      cli::cli_abort('{.fn {snake_class(self)}} only works with {.code coord_ip(curve = "hilbert")}.')
    }

    # validate ip aesthetic
    if (is.null(data$ip)) {
      data$ip <- coord$canvas_network
    } else if (is_ip_network_coords(data$ip)) {
      data$ip <- data$ip$ip
    }
    if (!is_ip_network(data$ip)) {
      cli::cli_abort(c(
        "The {.field ip} aesthetic of {.fn {snake_class(self)}} must be {.type {ip_network()}}.",
        "x" = "You supplied {.type {data$ip}}."
      ))
    }

    segments <- data %>%
      dplyr::distinct() %>%
      networks_to_squares(coord) %>%
      squares_to_sides() %>%
      sides_to_segments(coord) %>%
      dplyr::distinct()

    ggplot2::GeomSegment$draw_panel(segments, panel_params, coord,
                                    lineend = "round", na.rm = na.rm)
  },

  draw_key = ggplot2::draw_key_path
)

#' Translate IP networks to paths of squares, to visualize their Hilbert curves.
#'
#' @param data A data.frame with 3 columns: `ip`, `curve_order`, `closed`.
#'   There is 1 row per network.
#' @param coord The `CoordIp` coordinate system.
#' @return A data.frame with 4 additional columns: `xmin`, `ymin`, `xmax`, `ymax`.
#'   There is 1 row per square of the path.
#'
#' @noRd
networks_to_squares <- function(data, coord) {
  data %>%
    dplyr::mutate(prefix_curve = (2 * .data$curve_order) + prefix_length(coord$canvas_network)) %>%
    dplyr::filter(prefix_length(.data$ip) < .data$prefix_curve) %>%
    dplyr::mutate(network_curve = subnets(.data$ip, new_prefix = .data$prefix_curve)) %>%
    tidyr::unchop("network_curve") %>%
    dplyr::mutate(coords = network_to_cartesian(
      .data$network_curve,
      canvas_network = coord$canvas_network,
      pixel_prefix = coord$pixel_prefix,
      curve = coord$curve
    )) %>%
    dplyr::select(-"prefix_curve", -"network_curve") %>%
    tidyr::unnest("coords")
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

#' Translate paths of squares to square sides (N sides per square).
#' This accounts for path direction and closed/open endcaps.
#'
#' @param data A data.frame with 5 columns: `xmin`, `ymin`, `xmax`, `ymax`, `closed`.
#'   There is 1 row per square of the path.
#' @return A data.frame with 5 columns: `xmin`, `ymin`, `xmax`, `ymax`, `side`.
#'   There is 1 row per side of the path outline.
#'
#' @noRd
squares_to_sides <- function(data) {
  data %>%
    dplyr::mutate(
      xmid = (.data$xmin + .data$xmax) / 2,
      ymid = (.data$ymin + .data$ymax) / 2
    ) %>%
    dplyr::group_by(.data$ip) %>%
    dplyr::mutate(
      from = path_direction(.data$xmid, .data$ymid, dplyr::lag(.data$xmid), dplyr::lag(.data$ymid)),
      to = path_direction(.data$xmid, .data$ymid, dplyr::lead(.data$xmid), dplyr::lead(.data$ymid))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      from = translate_endpoints(.data$from, .data$to, .data$closed),
      to = translate_endpoints(.data$to, .data$from, .data$closed)
    ) %>%
    dplyr::select(-"xmid", -"ymid") %>%
    tidyr::expand_grid(side = factor(c("right", "left", "up", "down"))) %>%
    dplyr::filter(
      .data$side != dplyr::coalesce(.data$from, "endcap"),
      .data$side != dplyr::coalesce(.data$to, "endcap")
    ) %>%
    dplyr::select(-"from", -"to")
}

#' Translate square sides into line segments (1 segment per side).
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
    dplyr::select(-"xmin", -"ymin", -"xmax", -"ymax", -"side")
}
