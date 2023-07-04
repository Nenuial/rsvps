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
    dplyr::filter(hat_startlisten, status == "geplant") |>
    dplyr::mutate(
      von = lubridate::ymd(von),
      bis = lubridate::ymd(bis)
    ) |>
    dplyr::mutate(
      titre = withr::with_locale(
        new = c("LC_TIME" = "fr_CH.UTF-8"),
        code = glue::glue("{format(von, '%d %B %Y')} - {format(bis, '%d %B %Y')} : {ort}")
      )
    )
}

#' Get Events for speaker dressage selection
#'
#' @return A tibble
#' @export
get_fnch_sp_events_dr <- function() {
  today <- clock::date_today(zone = "Europe/Zurich")

  today |>
    clock::add_weeks(-3) |>
    get_fnch_events(
      format(clock::date_today(zone = "Europe/Zurich"), "%Y-12-31"),
      disziplin = "DR"
    ) |>
    dplyr::filter(hat_startlisten, status == "geplant") |>
    dplyr::mutate(
      von = lubridate::ymd(von),
      bis = lubridate::ymd(bis)
    ) |>
    dplyr::mutate(
      titre = withr::with_locale(
        new = c("LC_TIME" = "fr_CH.UTF-8"),
        code = glue::glue("{format(von, '%d %B %Y')} - {format(bis, '%d %B %Y')} : {ort}")
      )
    )
}

#' Return list of possible category choices
#'
#' @return A string vector
#' @export
get_fnch_sp_class_cat <- function() {
  c(
    "Pas de catégorie minimale" = "",
    "100" = "100",
    "110" = "110",
    "120" = "120",
    "130" = "130",
    "140" = "140"
  )
}

#' Return list of possible category choices
#'
#' @return A string vector
#' @export
get_fnch_sp_class_cat_dr <- function() {
  c(
    "Pas de catégorie minimale" = "",
    "FB" = "FB",
    "L" = "L",
    "M" = "M",
    "S" = "S"
  )
}

get_fnch_sp_class_min_dr <- function(class_min) {
  dplyr::case_match(
    class_min,
    "FB" ~ c(pkg.env$ep_ch_fb, pkg.env$ep_ch_l, pkg.env$ep_ch_m, pkg.env$ep_ch_s),
    "L"  ~ c(pkg.env$ep_ch_l, pkg.env$ep_ch_m, pkg.env$ep_ch_s),
    "M"  ~ c(pkg.env$ep_ch_m, pkg.env$ep_ch_s),
    "S"  ~ c(pkg.env$ep_ch_s),
  ) -> return_classes

  return_classes
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
get_fnch_sp_startlist <- function(eventid, classid, nb_years, nb_ranks, class_min, titles_min) {
  get_fnch_startlist(eventid, classid) |>
    dplyr::filter(typ == "starter") -> startlist

  rnotion::rni_get_database("9a51d07d37be4ea0ae29a8b41deba965") |>
    rnotion::rni_properties_tibble() |>
    dplyr::mutate(Date = lubridate::int_start(Date)) |>
    dplyr::filter(lubridate::year(Date) >= titles_min) |>
    dplyr::arrange(dplyr::desc(Date)) |>
    dplyr::mutate(
      Date = withr::with_locale(
        new = c("LC_TIME" = "fr_CH.UTF-8"),
        code = format(Date, "%d %B %Y")
      )
    ) -> titles

  safe_horse_results <- purrr::possibly(get_fnch_horse_jumping_results,
                                        otherwise = NULL)

  safe_horse_details <- purrr::possibly(get_fnch_horse_infos,
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
    dplyr::group_by(pferd_id) -> results_clean

  if(nrow(results_clean) > 0) {
    results_clean |>
      dplyr::group_modify(~ {
        .x |>
          dplyr::filter(
            reiter_id == startlist |>
              dplyr::filter(pferd_id == .y$pferd_id) |>
              dplyr::pull(reiter_id))
      }) |>
      dplyr::ungroup() -> results_clean
  }

  if (class_min != "") {
    results_clean |>
      dplyr::filter(kategorie_code != "SP/CS") |>
      dplyr::mutate(height = readr::parse_number(kategorie_code)) |>
      dplyr::filter(height >= as.numeric(class_min)) -> results_clean
  }

  # Function to filter each rider's titles and results
  row_details <- function(index) {
    horse_id <- startlist[[index, "pferd_id"]]
    rider_id <- startlist[[index, "reiter_id"]]

    titles |>
      dplyr::filter(Licence == rider_id) -> rider_titles

    title_table <- list()

    if (nrow(rider_titles) > 0) {
      htmltools::tagList(
        htmltools::h3("Titres"),
        rider_titles |>
          dplyr::mutate(horse = purrr::map(Passeport, rsvps::get_fnch_horse_infos)) |>
          tidyr::hoist(horse, Cheval = list("name")) |>
          dplyr::mutate(Rang = dplyr::case_when(
            Rang == 1 ~ "🥇",
            Rang == 2 ~ "🥈",
            Rang == 3 ~ "🥉",
            TRUE      ~ ""
          )) |>
          dplyr::select(Lieu, Date, Niveau, `Catégorie`, Cheval, Rang) |>
          reactable::reactable(
            class = "result-table",
            pagination = FALSE,
            language = reactable::reactableLang(noData = "Pas de résultats"),
            columns = list(
              Cheval = reactable::colDef(html = TRUE, cell = function(value, index) {
                htmltools::tags$a(
                  href = glue::glue("https://info.fnch.ch/#/resultate/pferde/{rider_titles$Passeport[index]}"),
                  target = "_blank",
                  value
                )
              })
            )
          )
      ) -> title_table
    }

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
        Lieu = ort, `Épreuve` = kategorie_code, `Barème` = wertung_code, Rang = rang) -> horse_results

    safe_horse_details(horse_id) -> horse_details

    tibble::tribble(
      ~Mère, ~Père, ~`Père de la mère`,
      horse_details$mutter_name,
      horse_details$vater_name,
      horse_details$vater_der_mutter_name
    ) -> horse_origins

    htmltools::div(
      class = "result-detail",
      htmltools::h3("Origines"),
      horse_origins |>
        reactable::reactable(
          class = "result-table",
          pagination = FALSE,
          language = reactable::reactableLang(noData = "Pas d'information")
        ),
      title_table,
      htmltools::h3("Classements"),
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
    dplyr::select(Num = startnummer, Cavalier = reiter_name, Cheval = pferd_name, Lieu = reiter_ort) |>
    reactable::reactable(
      pagination = FALSE,
      details = row_details,
      class = "start-table",
      columns = list(
        Cavalier = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.fnch.ch/#/resultate/reiter/{startlist$reiter_id[index]}"),
            target = "_blank",
            value
          )
        }),
        Cheval = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.fnch.ch/#/resultate/pferde/{startlist$pferd_id[index]}"),
            target = "_blank",
            value
          )
        })
      )
    )
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
get_fnch_sp_startlist_dr <- function(eventid, classid, nb_years, nb_ranks, class_min, titles_min) {
  get_fnch_startlist(eventid, classid) |>
    dplyr::filter(typ == "starter") -> startlist

  rnotion::rni_get_database("9a51d07d37be4ea0ae29a8b41deba965") |>
    rnotion::rni_properties_tibble() |>
    dplyr::mutate(Date = lubridate::int_start(Date)) |>
    dplyr::filter(lubridate::year(Date) >= titles_min) |>
    dplyr::arrange(dplyr::desc(Date)) |>
    dplyr::mutate(
      Date = withr::with_locale(
        new = c("LC_TIME" = "fr_CH.UTF-8"),
        code = format(Date, "%d %B %Y")
      )
    ) -> titles

  safe_horse_results <- purrr::possibly(get_fnch_horse_dressage_results,
                                        otherwise = NULL)

  safe_horse_details <- purrr::possibly(get_fnch_horse_infos,
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
    dplyr::group_by(pferd_id) -> results_clean

  if(nrow(results_clean) > 0) {
    results_clean |>
      dplyr::group_modify(~ {
        .x |>
          dplyr::filter(
            reiter_id == startlist |>
              dplyr::filter(pferd_id == .y$pferd_id) |>
              dplyr::pull(reiter_id))
      }) |>
      dplyr::ungroup() -> results_clean
  }

  if (class_min != "") {
    class_min_code <- get_fnch_sp_class_min_dr(class_min)
    results_clean |>
      dplyr::filter(kategorie_code %in% class_min_codes) -> results_clean
  }

  # Function to filter each rider's titles and results
  row_details <- function(index) {
    horse_id <- startlist[[index, "pferd_id"]]
    rider_id <- startlist[[index, "reiter_id"]]

    titles |>
      dplyr::filter(Licence == rider_id) -> rider_titles

    title_table <- list()

    if (nrow(rider_titles) > 0) {
      htmltools::tagList(
        htmltools::h3("Titres"),
        rider_titles |>
          dplyr::mutate(horse = purrr::map(Passeport, rsvps::get_fnch_horse_infos)) |>
          tidyr::hoist(horse, Cheval = list("name")) |>
          dplyr::mutate(Rang = dplyr::case_when(
            Rang == 1 ~ "🥇",
            Rang == 2 ~ "🥈",
            Rang == 3 ~ "🥉",
            TRUE      ~ ""
          )) |>
          dplyr::select(Lieu, Date, Niveau, `Catégorie`, Cheval, Rang) |>
          reactable::reactable(
            class = "result-table",
            pagination = FALSE,
            language = reactable::reactableLang(noData = "Pas de résultats"),
            columns = list(
              Cheval = reactable::colDef(html = TRUE, cell = function(value, index) {
                htmltools::tags$a(
                  href = glue::glue("https://info.fnch.ch/#/resultate/pferde/{rider_titles$Passeport[index]}"),
                  target = "_blank",
                  value
                )
              })
            )
          )
      ) -> title_table
    }

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
        Lieu = ort, `Programme` = kategorie_code, Rang = rang) -> horse_results

    safe_horse_details(horse_id) -> horse_details

    tibble::tribble(
      ~Mère, ~Père, ~`Père de la mère`,
      horse_details$mutter_name,
      horse_details$vater_name,
      horse_details$vater_der_mutter_name
    ) -> horse_origins

    htmltools::div(
      class = "result-detail",
      htmltools::h3("Origines"),
      horse_origins |>
        reactable::reactable(
          class = "result-table",
          pagination = FALSE,
          language = reactable::reactableLang(noData = "Pas d'information")
        ),
      title_table,
      htmltools::h3("Classements"),
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
    dplyr::select(Num = startnummer, Cavalier = reiter_name, Cheval = pferd_name, Lieu = reiter_ort) |>
    reactable::reactable(
      pagination = FALSE,
      details = row_details,
      class = "start-table",
      columns = list(
        Cavalier = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.fnch.ch/#/resultate/reiter/{startlist$reiter_id[index]}"),
            target = "_blank",
            value
          )
        }),
        Cheval = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.fnch.ch/#/resultate/pferde/{startlist$pferd_id[index]}"),
            target = "_blank",
            value
          )
        })
      )
    )
}
