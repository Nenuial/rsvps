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

#' Get rider address
#'
#' @param license A rider license id
#'
#' @return A string with rider name and address
#' @export
get_fnch_rider_address <- function(license) {
  rider <- get_fnch_riders() %>% dplyr::filter(Licence == license)

  return_text <- glue::glue("{rider %>% dplyr::pull(Licence)} {rider %>% dplyr::pull(FirstName)} {rider %>% dplyr::pull(LastName)} = {rider %>% dplyr::pull(Address)}, {rider %>% dplyr::pull(ZIP)} {rider %>% dplyr::pull(Place)}")

  return(return_text)
}

#' Get rider email
#'
#' @param license A rider license id
#'
#' @return A string with rider name and email
#' @export
get_fnch_rider_email <- function(license) {
  rider <- get_fnch_riders() %>% dplyr::filter(Licence == license)

  return_text <- glue::glue("{rider %>% dplyr::pull(FirstName)} {rider %>% dplyr::pull(LastName)} <{rider %>% dplyr::pull(Email)}>")

  return(return_text)
}

#' Get rider phone
#'
#' @param license A rider license id
#'
#' @return A string with rider name and mobile phone
#' @export
get_fnch_rider_mobile <- function(license) {
  rider <- get_fnch_riders() %>% dplyr::filter(Licence == license)

  return_text <- glue::glue("{rider %>% dplyr::pull(FirstName)} {rider %>% dplyr::pull(LastName)} <{rider %>% dplyr::pull(PhoneM)}>")

  return(return_text)
}
