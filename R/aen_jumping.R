#' Add data to startlist
#'
#' @param table The startlist
#' @param last_championship Date of the last championship
#' @param engagement_deadline Deadline of current championship
#'
#' @return The table with data
#' @export
aen_get_championship_data <- function(table, last_championship, engagement_deadline) {
  table |>
    dplyr::filter(!kein_start) |>
    dplyr::rowwise() |>
    dplyr::mutate(data = list(aen_get_championship_infos(reiter_id, pferd_id,
                                                         last_championship, engagement_deadline))) |>
    tidyr::unnest_wider(data) |>
    dplyr::select(
      c(reiter_id,
        reiter_vorname,
        reiter_name,
        mobile,
        pferd_id,
        pferd_name,
        lic,
        dplyr::starts_with("res"),
        dplyr::starts_with("ex"),
        sop
      )
    )
}

#' Get all the data needed for Championship checks
#'
#' @param rider_id The rider id
#' @param horse_id The horse id
#' @param last_championship Date of the last championship
#' @param engagement_deadline Deadline of current championship
#'
#' @return A list to be expanded in championship table
#' @export
aen_get_championship_infos <- function(rider_id, horse_id, last_championship, engagement_deadline) {
  rider_id |>
    rsvps::get_fnch_rider_infos() -> rider_infos

  rider_id |>
    rsvps::get_my_fnch_rider_details() -> rider_details

  horse_id |>
    rsvps::get_fnch_horse_jumping_results() -> horse_results

  # Height for all results
  horse_results |>
    dplyr::filter(klassiert) |>
    dplyr::mutate(height = readr::parse_number(kategorie_code, na = c("SP/CS"))) |>
    tidyr::drop_na(height) -> horse_results_clean

  # Qualifications = HORSE + RIDER results
  horse_results_clean |>
    dplyr::filter(
      datum > last_championship,
      reiter_id == rider_id
    ) |>
    dplyr::summarise(res90 = sum(height == 90 | height == 95),
                     res100 = sum(height == 100 | height == 105),
                     res110 = sum(height == 110 | height == 115),
                     res120 = sum(height == 120 | height == 125),
                     res130 = sum(height == 130 | height == 135),
                     res140 = sum(height >= 140),
                     res110p = sum(height >= 110),
                     res120p = sum(height >= 120)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) -> qualifications

  horse_results_clean |>
    dplyr::filter(
      lubridate::year(datum) >= lubridate::year(engagement_deadline) - 1
    ) |>
    dplyr::summarise(ex135 = sum(height >= 135),
                     exN135 = sum(height >= 140 | kategorie_code == "N135"),
                     ex140 = sum(height >= 140 )) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) -> exclusions

  horse_results_clean |>
    dplyr::filter(
      lubridate::year(datum) >= lubridate::year(engagement_deadline) - 1,
      datum <= engagement_deadline
    ) |>
    dplyr::summarise(sop = sum(gwp)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) -> sop


  return(c(list(mobile = rider_details$mobile, lic = rider_infos$lizenzen), qualifications |> as.list(), exclusions |> as.list(), sop |> as.list()))
}

#' Check qualifications for B/R Championship
#'
#' @param table The data table
#'
#' @return The data table with a checkout column
#' @export
aen_check_championship_br <- function(table) {
  # Championnat B/R
  # Critères qualification PAIRE (depuis dernier championnat) :
  # * classements 90 à 105 : 2
  #
  # Critères d'exclusion PAIRE (depuis dernier championnat) :
  # * classement 110+
  #
  # Critères d'exclusion CHEVAL (année en cours et année précédente) :
  # * classement 135+

  table |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        (res90 + res100) >= 2,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        res110p > 0,
        FALSE,
        checkout
      )
    ) |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        ex135 > 0,
        FALSE,
        checkout
      )
    )
}

#' Check qualifications for R Championship
#'
#' @param table The data table
#'
#' @return The data table with a checkout column
#' @export
aen_check_championship_r <- function(table) {
  # Championnat R
  # Critères qualification PAIRE (depuis dernier championnat) :
  # * classements 100/105  : 5
  # * classements 110/plus : 2
  #
  # Critères d'exclusion PAIRE (depuis dernier championnat) :
  # * plus de 3 classements (>= 4) 120+
  #
  # Critères d'exclusion CHEVAL (année en cours et année précédente) :
  # * classement 140+

  table |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        res100 >= 5 | res110p >= 2 | (res100 + res110p * 2) >= 5,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        res120p > 3,
        FALSE,
        checkout
      )
    ) |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        ex140 > 0,
        FALSE,
        checkout
      )
    )
}


#' Check qualifications for R/N Championship
#'
#' @param table The data table
#'
#' @return The data table with a checkout column
#' @export
aen_check_championship_rn <- function(table) {
  # Championnat R/N
  # Critères qualification PAIRE (depuis dernier championnat) :
  # * classements 110/115  : 5
  # * classements 120/125  : 2
  # * classements 130/plus : 1
  #
  # Critères de catégorie :
  # * licence N et plus de 1500 SoP => 135
  # * cheval classé N 135 dans l'année en cours et l'année précédente => 135

  table |>
    dplyr::mutate(
      checkout = dplyr::if_else(
        res110 >= 5 | res120 >= 2 | res130 >= 1 | res140 >= 1 | (res110 + res120 * 2) >= 5,
        TRUE,
        FALSE
      )
    )  |>
    dplyr::mutate(
      category = dplyr::if_else(
        (stringr::str_detect(lic, "SN") & sop > 1500) | exN135 > 0,
        135,
        125
      )
    )
}



