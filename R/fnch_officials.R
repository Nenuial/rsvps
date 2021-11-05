#' Get officials
#'
#' @return A dataframe
#' @export
get_fnch_officials <- function() {
  url <- glue::glue("https://info.fnch.ch/mitgliedschaften/offizielle.json?limit=100000")
  info <- jsonlite::fromJSON(url)

  return(info$mitgliedschaften)
}

#' Get officials full (with addresses)
#'
#' @return A dataframe
#' @export
get_fnch_officials_full <- function() {
  url <- glue::glue("https://my.fnch.ch/api/veranstaltersoftware/v2/getOffizielle?access_key=Z9346FA7&app_version=3.2.13")
  info <- jsonlite::fromJSON(url)

  while (is.null(info$data)) {
    Sys.sleep(10)
    info <- jsonlite::fromJSON(url)
  }

  return(info$data$offizielle)
}
