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

#' Install fonts on MacOS
#'
#' @param system_wide Install the fonts for all users?
#'
#' @return Move fonts to MacOS font folder
#' @export
fnch_install_fonts_macos <- function(system_wide = FALSE) {
  new_font_path <- "~/Library/Fonts/"
  if (system_wide) new_font_path <- "/Library/Fonts/"

  fs::dir_ls(path = fnch_file("fonts/")) |>
    purrr::walk(
      .f = ~fs::file_copy(
        path = .x,
        new_path = new_font_path,
        overwrite = TRUE
      )
    )
}
