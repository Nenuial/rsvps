#' Get FNCH events from calendar
#'
#' @param startdate A stardate
#' @param enddate An enddate
#' @param disziplin A discipline string
#' @param regionalverband A regional code
#'
#' @return List of events
#' @export
get_fnch_events <- function(startdate, enddate, disziplin = "", regionalverband = c(), eventtyp = c(), typ = "") {
  if(length(eventtyp) > 0) {
    eventtyp <- return_event_typ_id(eventtyp)
    eventtyp <- glue::glue('"typ":', '{jsonlite::toJSON(eventtyp)}', ',')
  } else {
    eventtyp <- ""
  }
  if(length(regionalverband) > 0) regionalverband <- return_regional_id(regionalverband)

  filter <- glue::glue('{{',
                       '"von":', '"{startdate}T00:00:00.000Z"', ',',
                       '"bis":', '"{enddate}T00:00:00.000Z"', ',',
                       '"disziplin":', '"{disziplin}"', ',',
                       '{eventtyp}',
                       '"regionalverband":', '{jsonlite::toJSON(regionalverband)}',
                       '}}')
  order <- "von"
  limit <- 5000

  httr2::request("https://info.fnch.ch") |>
    httr2::req_url_path_append("veranstaltungen.json") |>
    httr2::req_url_query(
      limit = limit,
      filter = filter,
      oder = order,
      typ = typ
    ) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() -> cal

  return(cal$veranstaltungen)
}

#' Map classes to event table
#'
#' @param event_table A dataframe from `get_fnch_events`
#'
#' @return A dataframe
#' @export
map_fnch_event_classes <- function(event_table) {
  safe_event_classes <- purrr::possibly(get_fnch_event_classes,
                                        otherwise = NULL)

  event_table |>
    dplyr::filter(hat_inserat & hat_resultate) |>
    dplyr::mutate(pruefungen = purrr::map(id, safe_event_classes)) |>
    tidyr::unnest(pruefungen, names_sep = "_")
}

#' Get all classes of an event
#'
#' @param eventid An event id
#'
#' @return List of classes
#' @export
get_fnch_event_classes <- function(eventid) {
  glue::glue("https://info.fnch.ch/ausschreibung/{eventid}.json") |>
    jsonlite::fromJSON() |>
    purrr::pluck("pruefungen") |>
    dplyr::mutate(prnum = readr::parse_number(stringr::str_remove(nummer, pattern = "Nr. "))) |>
    dplyr::select(prnum, modus_code, datum_ausschreibung = datum) -> ausschreibung

  glue::glue("https://info.fnch.ch/resultate/veranstaltungen/{eventid}.json") |>
    jsonlite::fromJSON() |>
    purrr::pluck("pruefungen") |>
    dplyr::mutate(prnum = readr::parse_number(stringr::str_remove(nummer, pattern = "Nr. "))) |>
    dplyr::left_join(ausschreibung, by = c("prnum")) -> pruefungen

  return(pruefungen)
}

#' Get all startlists of an event
#'
#' @param eventid An event id
#'
#' @return A tibble
#' @export
get_fnch_event_startlists <- function(eventid) {
  httr2::request("https://info.fnch.ch") |>
    httr2::req_url_path_append(glue::glue("startlisten/{eventid}.json")) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("startlisten") |>
    dplyr::filter(hat_startnummern)
}

#' Get a specific startlist
#'
#' @param eventid An event id
#' @param classid A class id
#'
#' @return A tibble
#' @export
get_fnch_startlist <- function(eventid, classid) {
  httr2::request("https://info.fnch.ch") |>
    httr2::req_url_path_append(glue::glue("startlisten/{eventid}.json")) |>
    httr2::req_url_query(startliste_id = classid, sprache = "de") |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON() |>
    purrr::pluck("zeilen") |>
    tidyr::hoist(
      "pferde",
      pferd_id = list(1L, "id"),
      pferd_name = list(1L, "name")
    )
}

#' Change regional federation abreviation to codes
#'
#' @param x A string or vector of strings
#'
#' @return An id or vector of ids
return_regional_id <- function(x) {
  dplyr::case_when(x == "AEN"  ~ 1,
                   x == "ASCJ" ~ 2,
                   x == "AVSH" ~ 3,
                   x == "FER"  ~ 4,
                   x == "FFSE" ~ 5,
                   x == "FGE"  ~ 6,
                   x == "FTSE" ~ 7,
                   x == "OKV"  ~ 8,
                   x == "PNW"  ~ 9,
                   x == "SCV"  ~ 10,
                   x == "ZKV"  ~ 11)
}


#' Change event types to codes
#'
#' @param x A string or vector of strings
#'
#' @return An id or vector of ids
return_event_typ_id <- function(x) {
  dplyr::case_when(x == "CA"        ~ 1,
                   x == "CAA"       ~ 2,
                   x == "CAN"       ~ 11,
                   x == "CC"        ~ 12,
                   x == "CCN"       ~ 25,
                   x == "CD"        ~ 26,
                   x == "CDN"       ~ 49,
                   x == "CE"        ~ 50,
                   x == "CEN"       ~ 65,
                   x == "CH"        ~ 66,
                   x == "CNC"       ~ 80,
                   x == "CR"        ~ 83,
                   x == "CS"        ~ 90,
                   x == "CSN"       ~ 135,
                   x == "CV"        ~ 136,
                   x == "CVN"       ~ 145,
                   x == "D"         ~ 147,
                   x == "SM"        ~ 183,
                   x == "SM/CA"     ~ 184,
                   x == "SM/CC"     ~ 185,
                   x == "SM/CC-J"   ~ 186,
                   x == "SM/CC-R"   ~ 187,
                   x == "SM/CC-Y"   ~ 188,
                   x == "SM/CD"     ~ 189,
                   x == "SM/CD-Ch"  ~ 241,
                   x == "SM/CD-J"   ~ 190,
                   x == "SM/CD-R"   ~ 191,
                   x == "SM/CD-U25" ~ 243,
                   x == "SM/CD-Y"   ~ 192,
                   x == "SM/CE"     ~ 193,
                   x == "SM/CE-J"   ~ 194,
                   x == "SM/CE-Y"   ~ 195,
                   x == "SM/CE-JY"  ~ 196,
                   x == "SM/CR"     ~ 197,
                   x == "SM/CS"     ~ 198,
                   x == "SM/CS-Ch"  ~ 240,
                   x == "SM/CS-J"   ~ 199,
                   x == "SM/CS-R"   ~ 200,
                   x == "SM/CS-U25" ~ 242,
                   x == "SM/CS-V"   ~ 201,
                   x == "SM/CS-Y"   ~ 202,
                   x == "SM/P D"    ~ 204,
                   x == "SM/P F"    ~ 205,
                   x == "SM/P M"    ~ 206,
                   x == "SM/P S"    ~ 207,
                   x == "SM/PROM"   ~ 208,
                   x == "SM/TREC"   ~ 209,
                   x == "SM/TREC-J" ~ 210,
                   x == "SM/V"      ~ 211,
                   x == "SM/VK"     ~ 212,
                   x == "SM/W"      ~ 213,
                   x == "TREC"      ~ 214,
                   x == "VK"        ~ 215)
}
