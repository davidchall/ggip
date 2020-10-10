# Internally, coord_ip() maps ip_address and ip_network vectors to Cartesian coords.
#
# From the user's perspective, ggip supports specifying positional aesthetics
# in two different ways:
#  * ggip layers expect an `ip` aesthetic and know how to extract the Cartesian coords:
#    * stat_summary_address(aes(ip = address))
#  * external layers expect `x` and `y` aesthetics, but might also use the
#    ip_address vector:
#    * geom_label(aes(x = address$x, y = address$y, label = address$ip))
#
# Theoretically, this could be achieved using nested data frames. That is,
# coord_ip() replaces the ip_address column with a data.frame(ip_address, x, y) column.
# However, ggplot2 uses length(column) to validate the data. For a data frame,
# length returns the number of columns instead of the number of rows.
# In summary, ggplot is currently incompatible with data frame columns.
#
# Instead, we define 2 new vctrs classes (ip_address_coords and ip_network_coords),
# which are effectively data frames whose length() returns the number of rows.
# To support the `address$x` syntax, they provide $ accessor functions.


# ip_address_coords ---------------------------------------------------

new_ip_address_coords <- function(ip = ip_address(), x = integer(), y = integer()) {
  vctrs::vec_assert(ip, ip_address())
  vctrs::vec_assert(x, integer())
  vctrs::vec_assert(y, integer())

  vctrs::new_rcrd(list(ip = ip, x = x, y = y), class = "ip_address_coords")
}

ip_address_coords <- function(ip = ip_address(), x = integer(), y = integer()) {
  vctrs::vec_cast(ip, ip_address())
  vctrs::vec_cast(x, integer())
  vctrs::vec_cast(y, integer())

  new_ip_address_coords(ip, x, y)
}

is_ip_address_coords <- function(x) {
  inherits(x, "ip_address_coords")
}

#' @export
format.ip_address_coords <- function(x, ...) {
  format(vctrs::field(x, "ip"), ...)
}

#' @export
as.character.ip_address_coords <- function(x, ...) {
  format(x, ...)
}

#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.ip_address_coords <- function(x, ...) {
  vctrs::vec_proxy_equal(vctrs::field(x, "ip"))
}

#' @export
`$.ip_address_coords` <- function(x, i, ...) {
  out <- vctrs::field(x, i)
  out
}

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_address_coords <- function(x) "identity"



# ip_network_coords ---------------------------------------------------

new_ip_network_coords <- function(ip = ip_network(),
                                  xmin = integer(), ymin = integer(),
                                  xmax = integer(), ymax = integer()) {
  vctrs::vec_assert(ip, ip_network())
  vctrs::vec_assert(xmin, integer())
  vctrs::vec_assert(ymin, integer())
  vctrs::vec_assert(xmax, integer())
  vctrs::vec_assert(ymax, integer())

  vctrs::new_rcrd(
    list(ip = ip, xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    class = "ip_network_coords"
  )
}

ip_network_coords <- function(ip = ip_network(),
                              xmin = integer(), ymin = integer(),
                              xmax = integer(), ymax = integer()) {
  vctrs::vec_cast(ip, ip_network())
  vctrs::vec_cast(xmin, integer())
  vctrs::vec_cast(ymin, integer())
  vctrs::vec_cast(xmax, integer())
  vctrs::vec_cast(ymax, integer())

  new_ip_network_coords(ip, xmin, ymin, xmax, ymax)
}

is_ip_network_coords <- function(x) {
  inherits(x, "ip_network_coords")
}

#' @export
format.ip_network_coords <- function(x, ...) {
  format(vctrs::field(x, "ip"), ...)
}

#' @export
as.character.ip_network_coords <- function(x, ...) {
  format(x, ...)
}

#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.ip_network_coords <- function(x, ...) {
  vctrs::vec_proxy_equal(vctrs::field(x, "ip"))
}

#' @export
`$.ip_network_coords` <- function(x, i, ...) {
  out <- vctrs::field(x, i)
  out
}

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ip_network_coords <- function(x) "identity"
