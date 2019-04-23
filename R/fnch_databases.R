#' Return the riders' database
#'
#' @return A dataframe with all FNCH riders
#' @export
get_fnch_riders <- function() {
  return(pkg.env$riders)
}

#' Return the horses' database
#'
#' @return A dataframe with all FNCH horses
#' @export
get_fnch_horses <- function() {
  return(pkg.env$horses)
}
