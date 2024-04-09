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
    dplyr::rowwise() |>
    dplyr::mutate(abrechnung_pdf = dokumente |>
                    dplyr::filter(stringr::str_detect(titel, "Décompte final")) |>
                    dplyr::pull(url)) |>
    dplyr::mutate(pdf_text = pdftools::pdf_text(abrechnung_pdf)) |>
    dplyr::mutate(starts = purrr::map_int(pdf_text, sum_my_fnch_taxes)) |>
    dplyr::group_by(veranstalter_name) |>
    dplyr::summarise(total_starts = sum(starts))
}

#' Get internal my.swiss-equestrian.ch calendar
#'
#' @param year The year of the calendar
#'
#' @return A tibble
#' @export
get_my_fnch_events_excel <- function(year) {
  call_my_fnch_api(
    glue::glue("https://my.swiss-equestrian.ch/backend/events/veranstaltungen/export_liste.xlsx?nur_eigene=false&sportjahre={year}&status=&sektion_rayon=&disziplinen=&regionalverbaende=&rv_anlaesse=&von=&bis=")
    ) |>
    httr2::resp_body_raw() |>
    writeBin(glue::glue("{tempdir()}/events.xlsx"))

  readxl::read_excel(glue::glue("{tempdir()}/events.xlsx"))
}

#' Get event table from my.swiss-equestrian.ch
#'
#' @param year An integer
#'
#' @return A dataframe
#' @export
get_my_fnch_events <- function(year) {
  call_my_fnch_api(glue::glue("https://my.swiss-equestrian.ch/backend/oas/veranstaltungen.json?page=1&limit=100&sortField=datum&sortOrder=ASC&nur_eigene=true&sportjahre={year}&status=&sektion_rayon=&disziplinen=&regionalverbaende=&rv_anlaesse=")) |>
    httr2::resp_body_json() |>
    purrr::pluck("veranstaltungen") |>
    purrr::transpose() |>
    tibble::as_tibble()
}

#' Get all details for event
#'
#' @param id Event id
#'
#' @return A list
#' @export
get_my_fnch_event_details <- function(id) {
  call_my_fnch_api(glue::glue("https://my.swiss-equestrian.ch/backend/oas/veranstaltungen/{id}.json")) |>
    httr2::resp_body_json()
}

#' Get all details for a person
#'
#' @param id Person id
#'
#' @return A list
#' @export
get_my_fnch_rider_details <- function(id) {
  call_my_fnch_api(glue::glue("https://my.swiss-equestrian.ch/reiter/autocomplete?term={id}")) |>
    httr2::resp_body_json() |>
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
#' @return A dataframe
get_my_fnch_dokumente <- function(id) {
  call_my_fnch_api(glue::glue("https://my.swiss-equestrian.ch/backend/oas/veranstaltungen/{id}/dokumente.json")) |>
    httr2::resp_body_json() |>
    purrr::pluck("dokumente", "veranstaltung") |>
    purrr::transpose() |>
    tibble::as_tibble()
}

#' Get credentials for my.swiss-equestrian.ch
#'
#' @return A list with id and token
get_my_fnch_credentials <- function() {
  list(
    auth_id = keyring::key_list("backend.swiss-equestrian.ch")[["username"]],
    auth_token = keyring::key_get("backend.swiss-equestrian.ch")
  )
}

#' Get login for my.swiss-equestrian.ch
#'
#' @return A list with id and token
get_my_fnch_login <- function() {
  list(
    login = keyring::key_list("my.swiss-equestrian.ch")[["username"]],
    password = keyring::key_get("my.swiss-equestrian.ch")
  )
}

#' Get cookie data for my.swiss-equestrian.ch
#'
#' @return A Coookie string
get_my_fnch_cookie <- function() {
  keyring::key_get("my.swiss-equestrian.ch", username = "Cookie")
}

#' Get authenticity token for my.swiss-equestrian.ch
#'
#' @return A string with authenticity token for login function
get_my_fnch_authenticity_token <- function() {
  httr::GET(url = "https://my.swiss-equestrian.ch/benutzer/sign_in") |>
    xml2::read_html() |>
    rvest::html_element('input[name="authenticity_token"]') |>
    rvest::html_attr("value")
}

#' Call my.swiss-equestrian.ch API
#'
#' @param url A string
#'
#' @return An httr response object
call_my_fnch_api <- function(url) {
  httr2::request(url) |>
    httr2::req_headers(
      Accept = "application/json",
      Origin = "https://my.swiss-equestrian.ch/",
      Referer = "https://my.swiss-equestrian.ch/",
      Cookie = get_my_fnch_cookie(),
      `x-auth-token` = get_my_fnch_credentials()[["auth_token"]],
      `x-auth-id` = get_my_fnch_credentials()[["auth_id"]],
      `x-auth-type` = "selfservice"
    ) |>
    httr2::req_perform()
}

#' Browse my.swiss-equestrian.ch
#'
#' @param url A string
#' @param query A list of query arguments
#'
#' @return An rvest response object
browse_my_fnch <- function(url, query = NULL) {
  login_creds <- list(
    "benutzer[email]" = get_my_fnch_login()$login[[1]],
    "benutzer[password]" = get_my_fnch_login()$password
  )

  session <- rvest::session("https://my.swiss-equestrian.ch/")

  session |>
    rvest::session_jump_to("https://my.swiss-equestrian.ch/benutzer/sign_in") |>
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
# https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getPersonen?access_key=GK2N3MHJ&app_version=3.2.4
# https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getOffizielle?access_key=GK2N3MHJ&app_version=3.2.4
# https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getVeranstaltung?access_key=GK2N3MHJ&app_version=3.2.4
