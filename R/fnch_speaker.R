#' Get Events for speaker selection
#'
#' @return A tibble
#' @export
get_fnch_sp_events <- function() {
  today <- clock::date_today(zone = "Europe/Zurich")

  today |>
    clock::add_weeks(-3) |>
    get_fnch_events(
      format(clock::date_today(zone = "Europe/Zurich"), "%Y-12-31"),
      disziplin = "SP"
    ) |>
    dplyr::filter(hat_startlisten)
}

#' Make startlist table
#'
#' @param eventid Event id
#' @param classid Classid
#' @param nb_years Number of years to go back
#' @param nb_ranks Number of ranks to consider
#'
#' @return A reactable object
#' @export
get_fnch_sp_startlist <- function(eventid, classid, nb_years, nb_ranks) {
  get_fnch_startlist(eventid, classid) |>
    dplyr::filter(typ == "starter") -> startlist

  safe_horse_results <- purrr::possibly(get_fnch_horse_jumping_results,
                                        otherwise = NULL)

  startlist |>
    dplyr::pull(pferd_id) |>
    purrr::map_df(safe_horse_results) -> results

  results |>
    dplyr::mutate(datum = lubridate::as_date(datum)) |>
    dplyr::filter(
      lubridate::year(datum) >=
        (lubridate::year(lubridate::today()) - (nb_years - 1))
    ) |>
    dplyr::filter(rang <= nb_ranks) |>
    dplyr::group_by(pferd_id) |>
    dplyr::group_modify(~ {
      .x |>
        dplyr::filter(
          reiter_id == startlist |>
            dplyr::filter(pferd_id == .y$pferd_id) |>
            dplyr::pull(reiter_id))
    }) |>
    dplyr::ungroup() -> results_clean

  row_details <- function(index) {
    horse_id <- startlist[[index, "pferd_id"]]

    results_clean |>
      dplyr::filter(pferd_id == horse_id) |>
      dplyr::mutate(
        date = withr::with_locale(
          new = c("LC_TIME" = "fr_CH.UTF-8"),
          code = strftime(datum, format = "%d %B %Y")
        )
      ) |>
      dplyr::select(
        Date = date,
        Lieu = ort, `Épreuve` = kategorie_code, Rang = rang) -> horse_results

    htmltools::div(
      class = "result-detail",
      horse_results |>
        reactable::reactable(
          class = "result-table",
          pagination = FALSE,
          language = reactable::reactableLang(noData = "Pas de résultats")
        )
    )
  }

  startlist |>
    dplyr::arrange(reihenfolge) |>
    dplyr::select(Num = startnummer, Lic = reiter_id, Cavalier = reiter_name,
                  Pass = pferd_id, Cheval = pferd_name) |>
    reactable::reactable(
      pagination = FALSE,
      details = row_details,
      class = "start-table"
    )
}
