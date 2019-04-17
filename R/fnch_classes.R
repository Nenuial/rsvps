#' Get results for all events
#'
#' @param event_obj An event object
#'
#' @return A dataframe with results for all events
#' @export
get_fnch_result_frame <- function(event_obj) {
  event_classes <- get_fnch_event_classes(event_obj$id, event_obj$ort)

  event_classes %>%
    purrr::map_df(map_class_results)
}

#' Get results for a specific class and event
#'
#' @param id ID of the class
#' @param eventid ID of the event
#'
#' @return A dataframe with the results of the specified class
#' @export
get_fnch_class_results <- function(id, eventid) {
  url <- glue::glue("https://info.fnch.ch/resultate/veranstaltungen/{eventid}.json?pruefung_id={id}")
  pru <- jsonlite::fromJSON(url)

  if(length(pru$resultate) == 0) return()

  res <- pru$resultate %>%
    tidyr::unnest(pferde, .sep = "_")

  return(res)
}

#' Function to map class results
#'
#' @param class_obj A class object
#'
#' @return A dataframe with results of class, function adds some additional info
map_class_results <- function(class_obj) {
  results <- get_fnch_class_results(class_obj$id, class_obj$eventid)

  if(length(results) == 0) return()

  results %<>%
    dplyr::mutate(kategorie_code = ifelse(kategorie_code %in% kur,
                                          kategorie_code,
                                          class_obj$cache_kategorie_code),
                  kategorie_nr = class_obj$nummer,
                  event_ort = class_obj$eventort)

  return(results)
}
