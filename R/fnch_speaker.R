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

#' Return list of possible dressage category choices
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

#' Return french dressage programmes
#'
#' @return A string vector
#' @keywords internal
get_fnch_sp_dr_programme_fr <- function(programme) {
  dplyr::case_match(
    programme,
    "GA01/40"   ~ "FB01/40",
    "GA02/60"   ~ "FB02/60",
    "GA03/40"   ~ "FB03/40",
    "GA04/60"   ~ "FB04/60",
    "GA05/40"   ~ "FB05/40",
    "GA06/60"   ~ "FB06/60",
    "GA07/40"   ~ "FB07/40",
    "GA08/60"   ~ "FB08/60",
    "GA09/40"   ~ "FB09/40",
    "GA10/60"   ~ "FB10/60",
    "GEORG"     ~ "St-Georges",
    "AChoix-GA" ~ "FB à choix",
    "AChoix-L"  ~ "L à choix",
    "AChoix-M"  ~ "M à choix",
    "AlaCarte"  ~ "À la carte",
    "LK"        ~ "Kür L",
    "MK"        ~ "Kür M",
    "GEORGK"    ~ "Kür St-Georges",
    .default = programme
  )
}

#' Function to get the dressage classes
#'
#' @param class_min The minimal class
get_fnch_sp_class_min_dr <- function(class_min) {
  return_classes <- c()

  if (class_min == "FB") {
    return_classes <- c(pkg.env$ep_ch_fb, pkg.env$ep_ch_l, pkg.env$ep_ch_m, pkg.env$ep_ch_s)
  } else if (class_min == "L") {
    return_classes <- c(pkg.env$ep_ch_l, pkg.env$ep_ch_m, pkg.env$ep_ch_s)
  } else if (class_min == "M") {
    return_classes <- c(pkg.env$ep_ch_m, pkg.env$ep_ch_s)
  } else if (class_min == "L") {
    return_classes <- ~ c(pkg.env$ep_ch_s)
  }

  return_classes
}

#' Retrieve title table from Notion
#'
#' @param titles_min Min year for titles
#'
#' @return A tibble
#' @export
get_fnch_sp_titles <- function(titles_min) {
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
    )
}

#' Get startlist data
#'
#' @param eventid Event ID
#' @param classid Class ID
#' @param nb_years Number of years
#' @param nb_ranks Number of rankings
#' @param class_min Mininum height for class
#'
#' @return A tibble
#' @export
get_fnch_sp_startlist_data <- function(eventid, classid, nb_years, nb_ranks, class_min, discipline = "jumping") {

  safe_horse_details <- purrr::possibly(rsvps::get_fnch_horse_infos,
                                        otherwise = NULL)
  safe_horse_results <- purrr::possibly(get_fnch_horse_jumping_results,
                                        otherwise = NULL)

  get_fnch_startlist(eventid, classid) |>
    dplyr::filter(typ == "starter") |>
    dplyr::mutate(horse_details = purrr::map(pferd_id, safe_horse_details)) |>
    tidyr::hoist(horse_details,
                 vater_name = list("vater_name"),
                 mutter_name = list("mutter_name"),
                 vater_der_mutter_name = list("vater_der_mutter_name")) |>
    dplyr::select(-horse_details) -> startlist

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

  if (class_min != "" & discipline == "jumping") {
    results_clean |>
      dplyr::filter(kategorie_code != "SP/CS") |>
      dplyr::mutate(height = readr::parse_number(kategorie_code)) |>
      dplyr::filter(height >= as.numeric(class_min)) -> results_clean
  } else if (class_min != "" & discipline == "dressage") {
    class_min_codes <- get_fnch_sp_class_min_dr(class_min)
    results_clean |>
      dplyr::filter(kategorie_code %in% class_min_codes) -> results_clean
  }

  return(list(startlist = startlist, results = results_clean, discipline = discipline))
}

#' Make startlist table
#'
#' @param startlist_data The startlist data
#' @param titles The titles data
#' @seealso [get_fnch_startlist_data()]
#'
#' @return A reactable object
#' @export
get_fnch_sp_startlist <- function(startlist_data, titles) {
  startlist = startlist_data$startlist
  results = startlist_data$results
  discipline = startlist_data$discipline

  startlist |>
    dplyr::arrange(reihenfolge) |>
    dplyr::select(Num = startnummer, Cavalier = reiter_name, Cheval = pferd_name, Lieu = reiter_ort) |>
    reactable::reactable(
      pagination = FALSE,
      details = \(x) get_fnch_sp_details(x, startlist, results, titles, discipline),
      class = "start-table",
      columns = list(
        Cavalier = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.swiss-equestrian.ch/#/resultate/reiter/{startlist$reiter_id[index]}"),
            target = "_blank",
            value
          )
        }),
        Cheval = reactable::colDef(html = TRUE, cell = function(value, index) {
          htmltools::tags$a(
            href = glue::glue("https://info.swiss-equestrian.ch/#/resultate/pferde/{startlist$pferd_id[index]}"),
            target = "_blank",
            value
          )
        })
      )
    )
}

#' Generate the details for each rider
#'
#' @param index The row index
#' @param startlist The startlist
#' @param results The results
#' @param titles The titles
get_fnch_sp_details <- function(index, startlist, results, titles, discipline) {
  starter <- startlist[index,]

  title_table <- get_fnch_sp_details_titles(starter[["reiter_id"]], titles)

  tibble::tribble(
    ~Mère, ~Père, ~`Père de la mère`,
    starter[["mutter_name"]],
    starter[["vater_name"]],
    starter[["vater_der_mutter_name"]],
  ) -> horse_origins

  results |>
    dplyr::filter(pferd_id == starter[["pferd_id"]]) |>
    get_fnch_clean_results(discipline) -> horse_results

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
    htmltools::h3("Résultats"),
    horse_results |>
      reactable::reactable(
        class = "result-table",
        pagination = FALSE,
        language = reactable::reactableLang(noData = "Pas de résultats")
      )
  )
}

#' Format the results and return the clean result table
#'
#' @param results The results
#' @param discipline The discipline
#'
#' @export
get_fnch_clean_results <- function(results, discipline) {
  results |>
    dplyr::mutate(
      date = withr::with_locale(
        new = c("LC_TIME" = "fr_CH.UTF-8"),
        code = strftime(datum, format = "%d %B %Y")
      )
    ) |>
    dplyr::mutate(
      rang = dplyr::if_else(
        klassiert,
        glue::glue("{rang} cl."),
        glue::glue("{rang}")
      )
    ) -> results

  if(discipline == "jumping") {
    results |>
      dplyr::select(
        Date = date,
        Lieu = ort, `Épreuve` = kategorie_code, `Barème` = wertung_code, Rang = rang) -> clean_results

    return(clean_results)
  } else if (discipline == "dressage") {
    results |>
      dplyr::mutate(kategorie_code = get_fnch_sp_dr_programme_fr(kategorie_code)) |>
      dplyr::select(
        Date = date,
        Lieu = ort, `Programme` = kategorie_code, Rang = rang
      ) -> clean_results

    return(clean_results)
  }
}

#' Return table with rider titles
#'
#' @param rider_id The rider id
#' @param titles The title table
get_fnch_sp_details_titles <- function(rider_id, titles) {
  titles |>
    dplyr::filter(Licence == rider_id) -> rider_titles

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
                href = glue::glue("https://info.swiss-equestrian.ch/#/resultate/pferde/{rider_titles$Passeport[index]}"),
                target = "_blank",
                value
              )
            })
          )
        )
    ) -> rider_titles

    return(rider_titles)
  }

  return(list())
}

#' Make PDF startlist
#'
#' @param startlist_data The startlist data
#' @param titles The titles data
#' @seealso [get_fnch_startlist_data()]
#'
#' @return A path to the pdf
#' @export
render_fnch_sp_startlist_pdf <- function(startlist_data, titles) {
  temporary_template <- fs::file_temp(ext = ".qmd")
  fs::file_copy(
    fnch_file("quarto/Speaker/Speaker_startlist.qmd"),
    temporary_template,
    overwrite = TRUE
  )

  quarto::quarto_render(
    input = temporary_template,
    execute_params = list(
      startlist = yyjsonr::write_json_str(startlist_data$startlist),
      results = yyjsonr::write_json_str(startlist_data$results),
      titles = yyjsonr::write_json_str(titles),
      discipline = startlist_data$discipline
    )
  )

  fs::path(
    fs::path_ext_remove(temporary_template),
    ext = "pdf"
  )
}
