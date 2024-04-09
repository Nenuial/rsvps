#' Function to map class results
#'
#' @param class_obj A dataframe with class ids
#'
#' @return A dataframe with class results
#' @export
map_fnch_class_results <- function(class_table) {
  safe_class_results <- purrr::possibly(get_fnch_class_results,
                                        otherwise = NULL)

  class_table |>
    dplyr::mutate(resultate = purrr::map2(pruefungen_id, id, safe_class_results)) |>
    tidyr::unnest(resultate, names_sep = "_")
}

#' Get results for a specific class and event
#'
#' @param id ID of the class
#' @param eventid ID of the event
#'
#' @return A dataframe with the results of the specified class
#' @export
get_fnch_class_results <- function(id, eventid) {
  url <- glue::glue("https://info.swiss-equestrian.ch/resultate/veranstaltungen/{eventid}.json?pruefung_id={id}")
  pru <- jsonlite::fromJSON(url)

  if (length(pru$resultate) == 0) return()

  res <- pru$resultate %>%
    tidyr::unnest(pferde, names_sep = "_")

  return(res)
}

#' Get Dressage FB levels
#'
#' @return A string vector
#' @export
get_fnch_dr_fb_levels <- function() {
  return(pkg.env$ep_ch_fb)
}

#' Get Dressage L levels
#'
#' @return A string vector
#' @export
get_fnch_dr_l_levels <- function() {
  return(pkg.env$ep_ch_l)
}

#' Get Dressage M levels
#'
#' @return A string vector
#' @export
get_fnch_dr_m_levels <- function() {
  return(pkg.env$ep_ch_m)
}

#' Get Dressage S levels
#'
#' @return A string vector
#' @export
get_fnch_dr_s_levels <- function() {
  return(pkg.env$ep_ch_s)
}

#' Get Dressage Kur levels
#'
#' @return A string vector
#' @export
get_fnch_dr_kur_levels <- function() {
  return(pkg.env$kur)
}
