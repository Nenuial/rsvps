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

  return(info$reiter)
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
