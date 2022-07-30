#' Get gwp infos for horseid
#'
#' @param horseid A passport id for a horse
#'
#' @return A dataframe of gwp data
#' @export
get_fnch_horse_gwp <- function(horseid) {
  url <- glue::glue("https://info.fnch.ch/resultate/pferde/{horseid}.json?tab=gwp")
  info <- jsonlite::fromJSON(url)

  if(length(info) == 0) return(
    tibble::tribble(
      ~disziplin_code, ~aktuelle_gewinnpunkte,
      "SP", 0
    )
  )

  return(info |> dplyr::select(1:2))
}

#' Get jumping results for horseid
#'
#' @param horseid A passport id for a horse
#'
#' @return A dataframe of jumping results
#' @export
get_fnch_horse_jumping_results <- function(horseid) {
  httr2::request("https://info.fnch.ch") |>
    httr2::req_url_path_append(glue::glue("resultate/pferde/{horseid}.json")) |>
    httr2::req_url_query(limit = 1000, tab = "springen") |>
    httr2::req_perform() |>
    httr2::resp_body_json() -> res

  if (length(res$resultate) == 0) return()

  res <- res$resultate |>
    purrr::transpose() |>
    tibble::as_tibble()

  return(res)
}

#' Get Horse Jumping GWP
#'
#' @param horseid A passport id for a horse
#' @param enddate The end date for the GWP period
#' @param startdate The start date for the GWP. Defaults to last year.
#'
#' @return An integer amount of points.
#' @export
get_fnch_horse_jumping_gwp <- function(horseid, enddate,
                                       startdate = floor_date(lubridate::today(), "year") - years(1)) {
  res <- get_fnch_horse_jumping_results(horseid)

  res %>%
    dplyr::mutate(datum = lubridate::ymd(datum)) %>%
    dplyr::filter(datum >= startdate,
                  datum <= enddate) %>%
    dplyr::summarise(gwp = sum(gwp)) %>%
    dplyr::pull(gwp) -> gwp

  return(gwp)
}
