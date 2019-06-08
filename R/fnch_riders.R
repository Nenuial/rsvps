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
