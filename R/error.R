# Use `stop_bad_aes_type()` when the aesthetic data has an unexpected type
stop_bad_aes_type <- function(layer, aes, expected) {
  abort(
    .subclass = "ggip_error_bad_aes_type",
    layer = layer, aes = aes, expected = expected
  )
}

#' @export
conditionMessage.ggip_error_bad_aes_type <- function(cnd) {
  glue::glue_data(
    cnd, "The `{aes}` aesthetic of `{layer}()` must be {expected}."
  )
}


# Use `stop_missing_aes()` when a required aesthetic was not specified
stop_missing_aes <- function(layer, aes) {
  abort(
    .subclass = "ggip_error_missing_aes",
    layer = layer, aes = aes
  )
}

#' @export
conditionMessage.ggip_error_missing_aes <- function(cnd) {
  glue::glue_data(
    cnd,
    "`{layer}()` must have the following aesthetics: ",
    '{glue::glue_collapse(glue::backtick(aes), sep = ", ", last = " and ")}.'
  )
}


# Use `stop_missing_coord()` when `coord_ip()` wasn't used
stop_missing_coord <- function() {
  abort(.subclass = "ggip_error_missing_coord")
}

#' @export
conditionMessage.ggip_error_missing_coord <- function(cnd) {
  "ggip plots must use the `coord_ip()` coordinate system."
}
