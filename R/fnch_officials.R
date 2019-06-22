#' Get officials
#'
#' @return A dataframe
#' @export
get_fnch_officials <- function() {
  url <- glue::glue("https://info.fnch.ch/mitgliedschaften/offizielle.json?limit=100000")
  info <- jsonlite::fromJSON(url)

  return(info$mitgliedschaften)
}
