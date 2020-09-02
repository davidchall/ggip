#' Hilbert curve outline
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param enclosed A logical scalar indicating whether the curve outline should
#'   have closed ends (default: `FALSE`).
#'
#' @section Aesthetics:
#' `geom_curve_outline()` understands the following aesthetics:
#'  - `ip`
#'  - `curve_order`
#'  - `alpha`
#'  - `colour`
#'  - `linetype`
#'  - `size`
#'
#' @export
geom_curve_outline <- function(mapping = NULL, data = "canvas", ...,
                               enclosed = FALSE, na.rm = FALSE) {
  if (data == "canvas") {
    data <- data.frame(group = 1)
  }

  ggplot2::layer(
    geom = GeomCurveOutline, data = data, mapping = mapping, stat = "identity",
    position = "identity", show.legend = FALSE, inherit.aes = FALSE,
    params = list(
      enclosed = enclosed,
      na.rm = na.rm,
      ...
    )
  )
}

GeomCurveOutline <- ggplot2::ggproto("GeomCurveOutline", ggplot2::Geom,
  default_aes = ggplot2::aes(
    ip = NULL,
    curve_order = 4,
    colour = "black",
    size = 0.5,
    linetype = 1,
    alpha = NA
  ),

  draw_panel = function(data, panel_params, coord, enclosed = FALSE, na.rm = FALSE) {

    if (!is_CoordIp(coord)) {
      abort("Must call coord_ip() when using ggip")
    }
    if (coord$curve != "hilbert") {
      abort('geom_curve_order is incompatible with coord_ip(curve = "morton")')
    }

    if (is.null(data$ip)) {
      data$ip <- coord$canvas_network
    }

    lines <- data %>%
      dplyr::rowwise(-ip, -curve_order) %>%
      dplyr::summarize(generate_curve_data(ip, curve_order, coord, enclosed))

    ggplot2::GeomSegment$draw_panel(lines, panel_params, coord,
                                    lineend = "round", na.rm = na.rm)
  },

  draw_key = ggplot2::draw_key_path
)

generate_curve_data <- function(network, curve_order, coord, enclosed) {
  curve_prefix <- (2 * curve_order) + prefix_length(coord$canvas_network)

  if (curve_prefix > prefix_length(network)) {
    tibble::tibble(network = subnets(network, new_prefix = curve_prefix)) %>%
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
    dplyr::full_join(tibble::tibble(side = sides), by = character()) %>%
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
