#' Stop because aesthetic data has an unexpected type
#' @noRd
stop_bad_aes_type <- function(layer, aes, expected) {
  abort(
    class = "ggip_error_bad_aes_type",
    layer = layer, aes = aes, expected = expected
  )
}

#' @export
conditionMessage.ggip_error_bad_aes_type <- function(c) {
  glue::glue_data(
    c, "The `{aes}` aesthetic of `{layer}()` must be a vector with type <{expected}>."
  )
}


#' Stop because a required aesthetic was not specified
#' @noRd
stop_missing_aes <- function(layer, aes) {
  abort(
    class = "ggip_error_missing_aes",
    layer = layer, aes = aes
  )
}

#' @export
conditionMessage.ggip_error_missing_aes <- function(c) {
  glue::glue_data(
    c,
    "`{layer}()` must have the following aesthetics: ",
    '{glue::glue_collapse(glue::backtick(aes), sep = ", ", last = " and ")}.'
  )
}

#' Stop because coordinate system is missing
#' @noRd
stop_missing_coord <- function() {
  abort(
    message = "ggip plots must use the `coord_ip()` coordinate system.",
    class = "ggip_error_missing_coord"
  )
}
