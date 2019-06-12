#' Check qualification for the BR Championship
#'
#' @param participant A list with riderid and horseid
#' @param last_championship The date of the last championship
#'
#' @return A list of data points with qualification decision
#' @export
aen_check_br_championship <- function(participant, last_championship) {
  data <- aen_get_championship_data(participant$riderid, participant$horseid, last_championship)

  data$qualification_count = sum(data$qualifications[["res90"]],
                                 data$qualifications[["res100"]])

  data$decision <- dplyr::case_when(data$BR_110 > 0                ~ FALSE,
                                    data$BR_135 > 0                ~ FALSE,
                                    data$qualification_count < 2   ~ FALSE,
                                    TRUE                           ~ TRUE)

  data_table <- tibble::as_tibble(purrr::modify_if(data, ~length(.x) > 1, list))

  return(data_table)
}

#' Check qualification for the R Championship
#'
#' @param participant A list with riderid and horseid
#' @param last_championship The date of the last championship
#'
#' @return A list of data points with qualification decision
#' @export
aen_check_r_championship <- function(participant, last_championship) {
  data <- aen_get_championship_data(participant$riderid, participant$horseid, last_championship)

  data$qualification_count = sum(data$qualifications[["res100"]],
                                 data$qualifications[["res110"]] * 2,
                                 data$qualifications[["res120"]] * 2,
                                 data$qualifications[["res130"]] * 2

  )

  if(data$qualifications[["res110"]] >= 2) data$qualification_count <- data$qualification_count + 1

  data$decision <- dplyr::case_when(data$R_120 > 3                 ~ FALSE,
                                    data$R_140 > 0                 ~ FALSE,
                                    data$horse_gwp > 900           ~ FALSE,
                                    data$qualification_count < 5   ~ FALSE,
                                    TRUE                           ~ TRUE)

  data_table <- tibble::as_tibble(purrr::modify_if(data, ~length(.x) > 1, list))

  return(data_table)
}

#' Check qualification for RN Championship
#'
#' @param participant A list with riderid and horseid
#' @param last_championship The date of the last championship
#'
#' @return A list of data points with qualification decision
#' @export
aen_check_rn_championship <- function(participant, last_championship) {
  data <- aen_get_championship_data(participant$riderid, participant$horseid, last_championship)

  data$qualification_count = sum(data$qualifications[["res110"]],
                                 data$qualifications[["res120"]] * 2,
                                 data$qualifications[["res130"]] * 5,
                                 data$qualifications[["res140"]] * 5
  )

  data$degree <- dplyr::case_when(data$rider_national & data$horse_gwp > 1500  ~ 135,
                                  data$horse_gwp > 5000                        ~ 135,
                                  data$RN_135 > 0                              ~ 135,
                                  TRUE                                         ~ 125)

  data$decision <- dplyr::case_when(data$qualification_count < 5   ~ FALSE,
                                    TRUE                           ~ TRUE)

  data_table <- tibble::as_tibble(purrr::modify_if(data, ~length(.x) > 1, list))

  return(data_table)
}

#' Get data for championship qualification analysis
#'
#' @param riderid A licence id for a rider
#' @param horseid A passport id for a horse
#' @param last_championship The date of the last championship
#'
#' @return A list of data points for use in qualification functions
aen_get_championship_data <- function(riderid, horseid, last_championship) {
  rider_infos <- get_fnch_rider_infos(riderid)

  horse_gwp <- get_fnch_horse_gwp(horseid) %>%
    dplyr::filter(disziplin_code == "SP") %>%
    dplyr::pull("aktuelle_gewinnpunkte")

  get_fnch_horse_jumping_results(horseid) %>%
    dplyr::mutate(datum = readr::parse_date(datum)) %>%
    dplyr::filter(lubridate::year(datum) >= lubridate::year(lubridate::today()) - 1) %>%
    dplyr::mutate(height = readr::parse_number(kategorie_code, na = c("SP/CS"))) %>%
    tidyr::drop_na(height) -> results_clean

  results_clean %>%
    dplyr::filter(reiter_id == riderid,
                  datum > last_championship) -> pair_results

  pair_results %>%
    dplyr::filter(height >= 110) %>%
    dplyr::count() %>%
    as.integer() -> BR_110

  results_clean %>%
    dplyr::filter(height >= 135) %>%
    dplyr::count() %>%
    as.integer() -> BR_135

  pair_results %>%
    dplyr::filter(height >= 120) %>%
    dplyr::count() %>%
    as.integer() -> R_120

  results_clean %>%
    dplyr::filter(height >= 140) %>%
    dplyr::count() %>%
    as.integer() -> R_140

  results_clean %>%
    dplyr::filter(kategorie_code == "N135" | height >= 140) %>%
    dplyr::count() %>%
    as.integer() -> RN_135

  pair_results %>%
    dplyr::summarise(res90 = sum(height == 90 | height == 95),
                     res100 = sum(height == 100 | height == 105),
                     res110 = sum(height == 110 | height == 115),
                     res120 = sum(height == 120 | height == 125),
                     res130 = sum(height == 130 | height == 135),
                     res140 = sum(height >= 140)) -> qualifications

  data <- list(rider_id = riderid,
               horse_id = horseid,
               horse_gwp = horse_gwp,
               rider_name = paste(rider_infos$vorname, rider_infos$nachname),
               rider_age = lubridate::year(lubridate::today()) - lubridate::year(rider_infos$geboren_am),
               rider_national = stringr::str_detect(rider_infos$lizenzen, "SN"),
               rider_club = rider_infos$verein,
               qualifications = qualifications,
               BR_110 = BR_110,
               BR_135 = BR_135,
               R_120 = R_120,
               R_140 = R_140,
               RN_135 = RN_135)

  return(data)
}
