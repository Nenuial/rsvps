#' Get FNCH ranking list
#'
#' @param start A date for the start of the period
#' @param end A date for the end of the period
#' @param discipline A string, either "CS", "CD" or "CC"
#' @param canton A string for the canton
#'
#' @return A dataframe with the rankings
#' @export
get_fnch_ranking <- function(start, end, discipline = "CS", canton = "") {
  discipline <- return_discipline_id(discipline)

  filter <- glue::glue('{{',
                       '"von":', '"{start}T00:00:00.000Z"', ',',
                       '"bis":', '"{end}T00:00:00.000Z"', ',',
                       '"disziplin_id":', '{jsonlite::toJSON(discipline)}', ',',
                       '"kanton":', '"{canton}"',
                       '}}')
  order <- "von"
  limit <- 20000

  "https://info.swiss-equestrian.ch/" |> 
    httr2::request() |> 
    httr2::req_url_path_append("rankings/paare.json") |>
    httr2::req_url_query(limit = limit, order = order, filter = filter) |>
    httr2::req_perform() |> 
    httr2::resp_body_string() |> 
    jsonlite::fromJSON() -> results

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
