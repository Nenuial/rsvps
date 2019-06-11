#' Get gwp infos for horseid
#'
#' @param horseid A passport id for a horse
#'
#' @return A dataframe of gwp data
#' @export
get_fnch_horse_gwp <- function(horseid) {
  url <- glue::glue("https://info.fnch.ch/resultate/pferde/{horseid}.json?tab=gwp")
  info <- jsonlite::fromJSON(url) %>%
    dplyr::select(1:2)

  return(info)
}

#' Get jumping results for horseid
#'
#' @param horseid A passport id for a horse
#'
#' @return A dataframe of jumping results
#' @export
get_fnch_horse_jumping_results <- function(horseid) {

  url <- glue::glue("https://info.fnch.ch/resultate/pferde/{horseid}.json?limit=1000&tab=springen")
  res <- jsonlite::fromJSON(url)

  if (length(res$resultate) == 0) return()

  res <- res$resultate

  return(res)
}

