#' Get the latest Rosson version string
#'
#' @return A string
get_rosson_latest_version <- function() {
  access_key <- get_rosson_access_key_cached()

  httr::GET(
    url = glue::glue("https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getPersonen?access_key={access_key}&app_version=3")
  ) |>
    httr::content(as = "parsed") |>
    purrr::pluck("message") |>
    stringr::str_match(
      r"[Rosson\WVersion\W([\d\.]*).*]"
    ) |>
    {\(x) x[,2]}()
}

#' Get a Rosson access key
#'
#' @return A string
get_rosson_access_key <- function() {
  browse_events_fnch(
    url = "/regionalverband/veranstaltungen",
    query = list(
      "utf8" = "✓",
      "regionalverband_veranstaltung_filter[eigene]" = "false",
      "regionalverband_veranstaltung_filter[mit_status]" = "geplant",
      "regionalverband_veranstaltung_filter[mit_typ]" = 90,
      "regionalverband_veranstaltung_filter[mit_beginn]" = format(Sys.Date(), "%d.%m.%Y"),
      "commit" = "Filtern"
    )
  ) |>
    rvest::read_html() |>
    rvest::html_elements(css = ".veranstaltung-url") |>
    rvest::html_attr("href") |>
    magrittr::extract2(1) |>
    browse_events_fnch() |>
    rvest::read_html() |>
    rvest::html_table() |>
    magrittr::extract2(1) |>
    dplyr::filter(X1 == "Access key") |>
    dplyr::pull(X2)
}


#' Get Rosson rider data
#'
#' @return A dataframe
#' @export
get_rosson_riders <- function() {
  access_key <- get_rosson_access_key_cached()
  app_version <- get_rosson_latest_version_cached()

  url <- glue::glue("https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getPersonen?access_key={access_key}&app_version={app_version}")
  info <- jsonlite::fromJSON(url)

  while (is.null(info$data)) {
    Sys.sleep(10)
    info <- jsonlite::fromJSON(url)
  }

  return(info$data$personen)
}
