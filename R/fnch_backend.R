#' Get summary table of number of starters per event organizer
#'
#' @param year An integer
#'
#' @return A tibble
#' @export
get_my_fnch_events_summary <- function(year) {
  get_my_fnch_events(year) |>
    dplyr::filter(status == "durchgefuehrt") |>
    dplyr::mutate(details = purrr::map(id, get_my_fnch_event_details)) |>
    dplyr::mutate(dokumente = purrr::map(id, get_my_fnch_dokumente)) |>
    tidyr::hoist(details, veranstalter_name = list("forms", "veranstaltungsdaten", "veranstalter_name")) |>
    tidyr::hoist(dokumente, dokumente_veranstaltung = list("dokumente", "veranstaltung")) |>
    dplyr::rowwise() |>
    dplyr::mutate(abrechnung_pdf = dokumente_veranstaltung |>
                    dplyr::filter(titel == "Schlussabrechnung" | stringr::str_detect(titel, "Décompte final")) |>
                    dplyr::pull(url)) |>
    dplyr::mutate(pdf_text = pdftools::pdf_text(abrechnung_pdf)) |>
    dplyr::mutate(starts = purrr::map_int(pdf_text, sum_my_fnch_taxes)) |>
    dplyr::group_by(veranstalter_name) |>
    dplyr::summarise(total_starts = sum(starts))
}

#' Get event table from my.fnch.ch
#'
#' @param year An integer
#'
#' @return A dataframe
#' @export
get_my_fnch_events <- function(year) {
  call_my_fnch_api(glue::glue("https://backend.fnch.ch/selfservice/oas/veranstaltungen.json?page=1&limit=1000&nur_eigene=true&sportjahre={year}&disziplinen=&regionalverbaende=")) |>
    jsonlite::fromJSON() |>
    magrittr::extract2("veranstaltungen")
}

#' Get all details for event
#'
#' @param id Event id
#'
#' @return A list
#' @export
get_my_fnch_event_details <- function(id) {
  call_my_fnch_api(glue::glue("https://backend.fnch.ch/selfservice/oas/veranstaltungen/{id}.json")) |>
    jsonlite::fromJSON()
}

#' Get all details for a person
#'
#' @param id Person id
#'
#' @return A list
#' @export
get_my_fnch_rider_details <- function(id) {
  browse_my_fnch(
    url ="/personen/autocomplete",
    query = list(term = id)
  ) |>
    purrr::pluck("response") |>
    httr::content(as = "parsed") |>
    purrr::pluck(1) -> data

  list(
    id = data$id,
    name = data$name,
    address = data$adresse_block_daten[[3]],
    mobile = stringr::str_match(data$adresse_block_komplett, r"[M:\s([^\n]*)]")[,2],
    email = stringr::str_match(data$adresse_block_komplett, r"[.+@.+$]")[,1],
    zip = stringr::str_match(data$adresse_block_daten[[4]], r"[((?:\D{2}-)?\d{4,6})\W(.*)]")[,2],
    place = stringr::str_match(data$adresse_block_daten[[4]], r"[((?:\D{2}-)?\d{4,6})\W(.*)]")[,3]
  )
}

#' Sum the taxes pulled from the account pdf
#'
#' @param pdf_text A string
#'
#' @return An integer
sum_my_fnch_taxes <- function(pdf_text) {
  matches <- stringr::str_match_all(pdf_text, "Taxes de manifestation (\\d*)")

  sum(as.integer(matches[[1]][,2]))
}

#' Get document info for event
#'
#' @param id Event id
#'
#' @return A list
get_my_fnch_dokumente <- function(id) {
  call_my_fnch_api(glue::glue("https://backend.fnch.ch/selfservice/oas/veranstaltungen/{id}/dokumente.json")) |>
    jsonlite::fromJSON()
}

#' Get credentials for my.fnch.ch
#'
#' @return A list with id and token
get_my_fnch_credentials <- function() {
  list(
    auth_id = keyring::key_list("backend.fnch.ch")[["username"]],
    auth_token = keyring::key_get("backend.fnch.ch")
  )
}

#' Get login for my.fnch.ch
#'
#' @return A list with id and token
get_my_fnch_login <- function() {
  list(
    login = keyring::key_list("my.fnch.ch")[["username"]],
    password = keyring::key_get("my.fnch.ch")
  )
}

#' Get login for events.fnch.ch
#'
#' @return A list with id and token
get_events_fnch_login <- function() {
  list(
    login = keyring::key_list("events.fnch.ch")[["username"]],
    password = keyring::key_get("events.fnch.ch")
  )
}

#' Get authenticity token for my.fnch.ch
#'
#' @return A string with authenticity token for login function
get_my_fnch_authenticy_token <- function() {
  httr::GET(url = "https://my.fnch.ch/benutzer/sign_in") |>
    xml2::read_html() |>
    rvest::html_element('input[name="authenticity_token"]') |>
    rvest::html_attr("value")
}

#' Call my.fnch.ch API
#'
#' @param url A string
#'
#' @return A string
call_my_fnch_api <- function(url) {
  httr::GET(url = url,
            httr::add_headers(
              Origin = "https://my.fnch.ch",
              Referer = "https://my.fnch.ch/",
              'x-auth-token' = get_my_fnch_credentials()[["auth_token"]],
              'x-auth-id' = get_my_fnch_credentials()[["auth_id"]],
              'x-auth-type' = "selfservice"
            )) |>
    xml2::read_html() |>
    rvest::html_text()
}

#' Browse my.fnch.ch
#'
#' @param url A string
#' @param query A list of query arguments
#'
#' @return An rvest response object
browse_my_fnch <- function(url, query = NULL) {
  login_creds <- list(
    "benutzer[email]" = get_my_fnch_login()$login,
    "benutzer[password]" = get_my_fnch_login()$password
  )

  session <- rvest::session("https://my.fnch.ch/")

  session |>
    rvest::session_jump_to("https://my.fnch.ch/benutzer/sign_in") |>
    rvest::read_html() |>
    rvest::html_form() |>
    magrittr::extract2(1) |>
    rvest::html_form_set(!!!login_creds) -> login_form

  login_form$fields[6] <- NULL

  session |>
    rvest::session_submit(login_form, submit = "commit", httr::user_agent("Gecko")) -> logged_session

  logged_session |>
    rvest::session_jump_to(
      url = url,
      query = query
    )
}

#' Browse events.fnch.ch
#'
#' @param url A string
#' @param query A list of query arguments
#'
#' @return An rvest response object
browse_events_fnch <- function(url, query = NULL) {
  login_creds <- list(
    "regionalverband_benutzer[email]" = get_events_fnch_login()$login,
    "regionalverband_benutzer[password]" = get_events_fnch_login()$password
  )

  session <- rvest::session("https://events.fnch.ch/")

  session |>
    rvest::session_jump_to("https://events.fnch.ch/regionalverband/benutzer/sign_in") |>
    rvest::read_html() |>
    rvest::html_form() |>
    magrittr::extract2(1) |>
    rvest::html_form_set(!!!login_creds) -> login_form

  login_form$fields[6] <- NULL

  session |>
    rvest::session_submit(login_form, submit = "commit", httr::user_agent("Gecko")) -> logged_session

  logged_session |>
    rvest::session_jump_to(
      url = url,
      query = query
    )
}

# Rosson API
# https://my.fnch.ch/api/veranstaltersoftware/v2/getPersonen?access_key=GK2N3MHJ&app_version=3.2.4
# https://my.fnch.ch/api/veranstaltersoftware/v2/getOffizielle?access_key=GK2N3MHJ&app_version=3.2.4
# https://my.fnch.ch/api/veranstaltersoftware/v2/getVeranstaltung?access_key=GK2N3MHJ&app_version=3.2.4
