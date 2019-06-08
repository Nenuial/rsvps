#' Get FNCH ranking list
#'
#' @param year_start A date for the start of the period
#' @param year_end A date for the end of the period
#' @param discipline A string, either "CS", "CD" or "CC"
#'
#' @return A dataframe with the rankings
#' @export
get_fnch_ranking <- function(year_start, year_end, discipline = "CS") {
  discipline <- return_discipline_id(discipline)

  filter <- glue::glue('{{',
                       '"von":', '"{year_start}T00:00:00.000Z"', ',',
                       '"bis":', '"{year_end}T00:00:00.000Z"', ',',
                       '"disziplin_id":', '{jsonlite::toJSON(discipline)}',
                       '}}')
  order <- "von"
  limit <- 20000

  url <- "https://info.fnch.ch/rankings/paare.json"
  url %<>% urltools::param_set(key = "limit", value = limit)
  url %<>% urltools::param_set(key = "filter", value = filter)
  url %<>% urltools::param_set(key = "order", value = order)

  results <- jsonlite::fromJSON(url)

  return(results$rankings)
}


#' Change discipline abreviation to code
#'
#' @param x A string, either "CS", "CD" or "CC"
#'
#' @return An integer or vector of integers
return_discipline_id <- function(x) {
  dplyr::case_when(x == "CS" ~ 6,
                   x == "CD" ~ 2,
                   x == "CC" ~ 3)
}
