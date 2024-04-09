#' Get officials
#'
#' @return A dataframe
#' @export
get_fnch_officials <- function() {
  url <- glue::glue("https://info.swiss-equestrian.ch/mitgliedschaften/offizielle.json?limit=100000")
  info <- jsonlite::fromJSON(url)

  return(info$mitgliedschaften)
}

#' Get officials full (with addresses)
#'
#' @return A dataframe
#' @export
get_fnch_officials_full <- function() {
  access_key <- get_rosson_access_key_cached()
  app_version <- get_rosson_latest_version_cached()

  url <- glue::glue("https://my.swiss-equestrian.ch/api/veranstaltersoftware/v2/getOffizielle?access_key={access_key}&app_version={app_version}")
  info <- jsonlite::fromJSON(url)

  while (is.null(info$data)) {
    Sys.sleep(10)
    info <- jsonlite::fromJSON(url)
  }

  return(info$data$offizielle)
}

#' Write CSV official data file for LDAP
#'
#' @return Nothing (writes csv file)
#' @export
write_fnch_officials_csv <- function() {
  officials <- get_fnch_officials_full()

  officials |>
    dplyr::mutate(title = purrr::map_chr(
      rollen,
      ~stringr::str_c(.x$rolle_bezeichnung_fr, collapse = ", ")
    )) |>
    dplyr::select(-rollen) |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("telefon"), ~dplyr::if_else(is.na(.x), "", .x))
    ) |>
    purrr::pmap_df(map_fnch_officials_ldap) -> ldap_csv

  ldap_csv |>
    readr::write_delim(file = "ldap_officials.csv", quote = "all", delim = ";")
}

#' Format the official data for LDAP
#'
#' @param ... Data from full official database
#'
#' @return A list with the parameters for the LDAP CSV
#' @keywords internal
map_fnch_officials_ldap <- function(...) {
  official <- list(...)

  list(
    "ID" = official$id,
    "UID" = official$id,
    "endOfValidity" = "31/12/2050",
    "SN" = paste(official$nachname, official$vorname),
    "CN" = janitor::make_clean_names(paste0(official$nachname, official$vorname)),
    "GIVENNAME" = official$vorname,
    "TITLE" = official$title,
    "MAIL" = official$email,
    "STREET" = official$strasse,
    "L" = official$ort,
    "POSTALCODE" = official$postleitzahl,
    "MOBILE" = official$telefon_mobil,
    "HOMEPHONE" = official$telefon_privat,
    "TELEPHONENUMBER" = official$telefon_arbeit
  )
}
