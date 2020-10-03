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
#'  - `curve_order`: How nested is the curve? (default: `4`).
#'  - `enclosed`: Should the curve outline have closed ends? (default: `FALSE`).
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
#' p + geom_hilbert_outline(ip = ip_network("128.0.0.0/4"))
#'
#' # increased nesting
#' p + geom_hilbert_outline(curve_order = 5)
#'
#' # show multiple networks
#' df <- data.frame(
#'   ip = ip_network(c("0.0.0.0/2", "128.0.0.0/4")),
#'   curve_order = c(4, 5)
#' )
#' p + geom_hilbert_outline(aes(ip = ip, curve_order = curve_order), data = df)
#' @export
geom_hilbert_outline <- function(mapping = NULL, data = NULL, ...,
                                 na.rm = FALSE) {
  # can use layer without any data
  if (is.null(data)) {
    data <- ensure_nonempty_data
  }

  ggplot2::layer(
    geom = GeomHilbertOutline, data = data, mapping = mapping, stat = "identity",
    position = "identity", show.legend = FALSE, inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomHilbertOutline <- ggplot2::ggproto("GeomHilbertOutline", ggplot2::Geom,
  default_aes = ggplot2::aes(
    ip = NULL,
    curve_order = 4,
    enclosed = FALSE,
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
      abort(glue::glue('`geom_hilbert_outline()` requires `coord_ip(curve = "hilbert")`.'))
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

    lines <- data %>%
      dplyr::distinct() %>%
      dplyr::rowwise(-ip, -curve_order) %>%
      dplyr::summarize(generate_curve_data(ip, curve_order, coord, enclosed)) %>%
      dplyr::distinct()

    ggplot2::GeomSegment$draw_panel(lines, panel_params, coord,
                                    lineend = "round", na.rm = na.rm)
  },

  draw_key = ggplot2::draw_key_path
)

generate_curve_data <- function(network, curve_order, coord, enclosed) {
  curve_prefix <- (2 * curve_order) + prefix_length(coord$canvas_network)

  if (curve_prefix > prefix_length(network)) {
    data.frame(network = subnets(network, new_prefix = curve_prefix)) %>%
      dplyr::mutate(network_to_cartesian(
        network,
        canvas_network = coord$canvas_network,
        pixel_prefix = coord$pixel_prefix,
        curve = coord$curve
      )) %>%
      squares_to_outline_sides(enclosed) %>%
      outline_sides_to_segments(coord)
  } else {
    data.frame(x = double(), y = double(), xend = double(), yend = double())
  }
}

find_direction <- function(x_from, y_from, x_to, y_to) {
  as.factor(dplyr::case_when(
    x_to > x_from ~ "right",
    x_to < x_from ~ "left",
    y_to > y_from ~ "up",
    y_to < y_from ~ "down",
    TRUE ~ "endpoint"
  ))
}

squares_to_outline_sides <- function(data, enclosed) {
  sides <- factor(c("left", "right", "up", "down"))
  levels(sides) <- c(levels(sides), "endpoint")

  data %>%
    dplyr::mutate(
      xmid = (.data$xmin + .data$xmax) / 2,
      ymid = (.data$ymin + .data$ymax) / 2,
      from = find_direction(.data$xmid, .data$ymid, dplyr::lag(.data$xmid), dplyr::lag(.data$ymid)),
      to = find_direction(.data$xmid, .data$ymid, dplyr::lead(.data$xmid), dplyr::lead(.data$ymid)),
      from = dplyr::case_when(
        enclosed ~ from,
        from != "endpoint" ~ from,
        to == "right" ~ factor("left"),
        to == "down" ~ factor("up")
      ),
      to = dplyr::case_when(
        enclosed ~ to,
        to != "endpoint" ~ to,
        from == "left" ~ factor("right"),
        from == "down" ~ factor("up")
      ),
    ) %>%
    dplyr::select(-.data$xmid, -.data$ymid) %>%
    dplyr::full_join(data.frame(side = sides), by = character()) %>%
    dplyr::filter(.data$side != .data$from, .data$side != .data$to) %>%
    dplyr::select(-.data$from, -.data$to)
}

outline_sides_to_segments <- function(data, coord) {
  data %>%
    dplyr::transmute(
      .data$side,
      x = dplyr::if_else(.data$side == "right", .data$xmax, .data$xmin),
      y = dplyr::if_else(.data$side == "up", .data$ymax, .data$ymin),
      xend = dplyr::if_else(.data$side == "left", .data$xmin, .data$xmax),
      yend = dplyr::if_else(.data$side == "down", .data$ymin, .data$ymax)
    ) %>%
    dplyr::transmute(
      x = dplyr::case_when(
        x %in% coord$limits$x ~ as.numeric(x),
        side == "right" ~ x + 0.5,
        TRUE ~ x - 0.5
      ),
      y = dplyr::case_when(
        y %in% coord$limits$y ~ as.numeric(y),
        side == "up" ~ y + 0.5,
        TRUE ~ y - 0.5
      ),
      xend = dplyr::case_when(
        xend %in% coord$limits$x ~ as.numeric(xend),
        side == "left" ~ xend - 0.5,
        TRUE ~ xend + 0.5
      ),
      yend = dplyr::case_when(
        yend %in% coord$limits$y ~ as.numeric(yend),
        side == "down" ~ yend - 0.5,
        TRUE ~ yend + 0.5
      )
    )
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
