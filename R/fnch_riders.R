#' Add rider info to result dataframe
#'
#' @param df A dataframe produced by `map_fnch_class_results`
#'
#' @return A dataframe
#' @export
add_rider_infos <- function(df) {
  safe_rider_infos <- purrr::possibly(get_fnch_rider_infos,
                                      otherwise = NULL)

  rider_additional <- get_fnch_riders() |>
    dplyr::rename_with(~glue::glue("reiter_zusatz_{.x}"))

  df |>
    dplyr::mutate(reiter = purrr::map(resultate_reiter_id, safe_rider_infos)) |>
    tidyr::unnest(reiter, names_sep = "_") |>
    dplyr::left_join(rider_additional, by = c("resultate_reiter_id" = "reiter_zusatz_id"))
}

#' Get general rider table
#'
#' @return A dataframe
#' @export
get_fnch_riders <- function() {
  url <- "https://info.fnch.ch/resultate/reiter.json?limit=100000"

  info <- jsonlite::fromJSON(url)

  return(info$reiter)
}

#' Get general infos for riderid
#'
#' @param riderid A licence id for a rider
#'
#' @return A list of rider infos
#' @export
get_fnch_rider_infos <- function(riderid) {
  url <- glue::glue("https://info.fnch.ch/resultate/reiter/{riderid}.json")
  info <- jsonlite::fromJSON(url)

  if(is.null(info$reiter$kanton)) info$reiter$kanton <- ""
  if(is.null(info$reiter$alterskategorie)) info$reiter$alterskategorie <- ""

  return(info$reiter |> tibble::as_tibble())
}

#' Get gwp infos for riderid
#'
#' @param riderid A licence id for a rider
#'
#' @return A dataframe of gwp data
#' @export
get_fnch_rider_gwp <- function(riderid) {
  url <- glue::glue("https://info.fnch.ch/resultate/reiter/{riderid}.json?tab=gwp")
  info <- jsonlite::fromJSON(url) %>%
    dplyr::select(1:2)

  return(info)
}

#' Get jumping results for riderid
#'
#' @param riderid A licence id for a rider
#'
#' @return A dataframe of jumping results
#' @export
get_fnch_rider_jumping_results <- function(riderid) {

  url <- glue::glue("https://info.fnch.ch/resultate/reiter/{riderid}.json?limit=1000&tab=springen")
  res <- jsonlite::fromJSON(url)

  if (length(res$resultate) == 0) return()

  res <- res$resultate

  return(res)
}
