#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
magrittr::`%<>%`

#' @importFrom rlang !!
#' @export
rlang::`!!`

#' A shortcut function for the system.file to the rsvps package
#'
#' @return A path
#'
#' @export
fnch_file = function(...) {
  system.file(..., package = 'rsvps', mustWork = TRUE)
}
