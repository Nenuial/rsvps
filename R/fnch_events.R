#' Get FNCH events from calendar
#'
#' @param startdate A stardate
#' @param enddate An enddate
#' @param disziplin A discipline string
#' @param regionalverband A regional code
#'
#' @return List of events
#' @export
get_fnch_events <- function(startdate, enddate, disziplin = "", regionalverband = c(), typ = "") {
  if(length(regionalverband) > 0) regionalverband <- return_regional_id(regionalverband)

  filter <- glue::glue('{{',
                       '"von":', '"{startdate}T00:00:00.000Z"', ',',
                       '"bis":', '"{enddate}T00:00:00.000Z"', ',',
                       '"disziplin":', '"{disziplin}"', ',',
                       '"regionalverband":', '{jsonlite::toJSON(regionalverband)}',
                       '}}')
  order <- "von"
  limit <- 5000

  url <- "https://info.fnch.ch/veranstaltungen.json"
  url %<>% urltools::param_set(key = "limit", value = limit)
  url %<>% urltools::param_set(key = "filter", value = filter)
  url %<>% urltools::param_set(key = "order", value = order)
  url %<>% urltools::param_set(key = "typ", value = typ)

  cal <- jsonlite::fromJSON(url)

  return(cal$veranstaltungen)
}

#' Get all classes of an event
#'
#' @param eventid An event id
#' @param eventort The place of the event
#'
#' @return List of classes
#' @export
get_fnch_event_classes <- function(eventid, eventort, classfilter = 'modus_code == "O"') {
  url <- glue::glue("https://info.fnch.ch/ausschreibung/{eventid}.json")
  aus <- jsonlite::fromJSON(url)
  aus <- aus$pruefungen %>%
    dplyr::mutate(prnum = readr::parse_number(nummer)) %>%
    dplyr::select(prnum, modus_code, datum_ausschreibung = datum)


  url <- glue::glue("https://info.fnch.ch/resultate/veranstaltungen/{eventid}.json")
  ver <- jsonlite::fromJSON(url)
  ver <- ver$pruefungen %>%
    dplyr::mutate(prnum = readr::parse_number(nummer),
                  eventid = eventid,
                  eventort = eventort) %>%
    dplyr::left_join(aus, by = c("prnum")) %>%
    dplyr::filter(!!rlang::parse_expr(classfilter))

  return(ver)
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
