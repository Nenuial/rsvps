#' Run Plumber API
#'
#' @param port An integer: the port to run the API on
#' @param host A string: the host to bind to
#' @param ... Options passed to \code{plumber::plumb()$run()}
#'
#' @return A running Plumber API
#' @export
run_api <- function(port = 8008, host = '0.0.0.0', ...) {
  #plumber::plumb(dir = system.file("plumber", package = "rsvps"))$run(...)
  plumber::pr(system.file("plumber", "api.R", package = "rsvps")) %>%
    plumber::pr_run(port = port, host = host, ...)
}

#' Plumber FNCH calendar
#'
#' @param year An integer: the calendar year
#' @param federation A string: the regional federation code. FER returns a combined set.
#'
#' @return A string: ICS calendar
pr_fnch_calendar <- function(year, federation) {
  if(federation == "FER") federation <- c("AEN", "ASCJ", "AVSH", "FFSE", "FGE", "SCV")

  get_fnch_ics_calendar(start = glue::glue("{year}-01-01"),
                               end = glue::glue("{year}-12-31"),
                               federation = c(federation)) %>%
    stringr::str_c(collapse = "\n")
}

#' Plumber FNCH full calendar
#'
#' @param federation A string: the regional federation code. FER returns a combined set.
#'
#' @return A string: ICS calendar
pr_fnch_full_calendar <- function(federation) {
  if(federation == "FER") federation <- c("AEN", "ASCJ", "AVSH", "FFSE", "FGE", "SCV")

  year <- as.integer(format(Sys.Date(), "%Y"))

  get_fnch_ics_calendar(start = glue::glue("{year-1}-01-01"),
                               end = glue::glue("{year+1}-12-31"),
                               federation = c(federation)) %>%
    stringr::str_c(collapse = "\n")
}

#' Plumber FNCH web calendar
#'
#' @param federation A string: the regional federation code. FER returns a combined set.
#'
#' @return A string: ICS calendar
pr_fnch_web_calendar <- function(federation) {
  if(federation == "FER") federation <- c("AEN", "ASCJ", "AVSH", "FFSE", "FGE", "SCV")

  year <- as.integer(format(Sys.Date(), "%Y"))

  get_fnch_ics_calendar(start = glue::glue("{year}-01-01"),
                               end = glue::glue("{year+1}-12-31"),
                               federation = c(federation),
                               with_links = TRUE) %>%
    stringr::str_c(collapse = "\n")
}

#' Plumber FNCH excel calendar
#'
#' @param year An integer: the year for the desired calendar
#'
#' @return A binary excel file
pr_fnch_xls_calendar <- function(year) {
  filename <- tempfile()
  write_fnch_calendar(path = filename, year = year)
  readBin(filename, "raw", n = file.info(filename)$size)
}
