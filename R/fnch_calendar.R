#' Write ICS calendar
#'
#' @param start A string for the start date (format yyyy-mm-dd)
#' @param end A string for the end date (format yyyy-mm-dd)
#' @param federation A string vector with the desired federations
#' @param path A path to write the ICS file to
#'
#' @export
write_fnch_ics_calendar <- function(start, end, federation, path) {
  get_fnch_ics_calendar(start, end, federation) %>%
    readr::write_lines(path)
}

#' Get ICS calendar
#'
#' @param start A string for the start date (format yyyy-mm-dd)
#' @param end A string for the end date (format yyyy-mm-dd)
#' @param federation A string vector with the desired federations
#'
#' @return An ICS calendar in string format
#' @export
get_fnch_ics_calendar <- function(start, end, federation, with_links = F) {
  get_fnch_events(start, end, regionalverband = federation) %>%
    dplyr::mutate(von = lubridate::ymd(von),
                  bis = lubridate::ymd(bis) + lubridate::days(1),
                  disp_links = with_links,
                  website_text = ifelse(is.na(website), "",
                                   glue::glue("<br/><a href='{website}' target='_blank'>Lien vers le site de l'organisateur</a>")),
                  Start = format(von, format = "%Y/%m/%d"),
                  End = format(bis, format = "%Y/%m/%d"),
                  Summary = glue::glue("{typ_code} {ort} ({kanton})"),
                  Description = ifelse(disp_links,
                                       glue::glue("{stringr::str_trim(stringr::str_replace_all(vorgesehene_pruefungen, '[\r\n]' , ' '))}<br/><a href='https://info.fnch.ch/#/veranstaltungskalender/ausschreibung/{id}' target='_blank'>Lien vers le portail FNCH</a>{website_text}"),
                                       glue::glue("{stringr::str_trim(stringr::str_replace_all(vorgesehene_pruefungen, '[\r\n]' , ' '))}")),
                  URL = glue::glue("https://info.fnch.ch/#/veranstaltungskalender/ausschreibung/{id}")) %>%
    dplyr::select(Start, End, Summary, Description, URL) %>%
    purrr::pmap_dfr(create_ics_cal) %>%
    calendar::ic_character() %>%
    stringr::str_remove("T000000")
}

#' Write calendar
#'
#' @param path A path
#' @param year An integer
#' @param federations A vector of federation abreviations
#'
#' @export
write_fnch_calendar <- function(path, year, federations = c("AEN", "ASCJ", "AVSH", "FFSE", "FGE", "SCV")) {
  get_clean_calendar(year) %>%
    dplyr::filter(`Association régionale` %in% federations) -> calendar

  selected_columns <- c("Lieu", "de", "à", "Canton", "Association régionale",
                        "Type", "Disciplines", "Epreuves prévues",
                        "Président CO", "Téléphone M")

  calendar |>
    dplyr::arrange(de, `à`) |>
    dplyr::select(tidyselect::all_of(selected_columns)) -> calendar_filtered

  pkg.env$start_row <- 1
  pkg.env$wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(pkg.env$wb, "Calendrier")
  openxlsx::writeData(pkg.env$wb, "Calendrier", calendar_filtered)


  get_event_type_colors() %>%
    purrr::pwalk(~apply_event_colors(pkg.env$wb, "Calendrier", ...))

  openxlsx::saveWorkbook(pkg.env$wb, file = path, overwrite = TRUE)
}

#' Write calendar
#'
#' @param path A path
#' @param year An integer
#' @param federations A vector of federation abreviations
#'
#' @export
write_fnch_week_calendar <- function(path, year, federations = c("AEN", "ASCJ", "AVSH", "FFSE", "FGE", "SCV", "ZKV")) {
  get_clean_calendar(year) %>%
    dplyr::filter(`Association régionale` %in% federations) -> calendar

  pkg.env$start_row <- 1
  pkg.env$wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(pkg.env$wb, "Calendrier")

  almanac::weekly(since = glue::glue("{year}-01-01"), until = glue::glue("{year}-12-31")) %>%
    almanac::recur_on_wday("Sunday") %>%
    almanac::alma_events() %>%
    purrr::walk(~create_week_table(calendar, .x, "Calendrier"))

  get_event_type_colors() %>%
    purrr::pwalk(~apply_event_colors(pkg.env$wb, "Calendrier", ...))

  openxlsx::saveWorkbook(pkg.env$wb, file = path, overwrite = TRUE)
}

#' Write collision calendar for federation
#'
#' @param path A path
#' @param year An integer
#' @param federation A string
#'
#' @export
write_fnch_collision_calendar <- function(path, year, federation) {
  calendar <- get_clean_calendar(year)

  pkg.env$start_row <- 1
  pkg.env$wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(pkg.env$wb, "Calendrier")

  calendar %>%
    dplyr::filter(`Association régionale` == federation) -> federation_calendar

  federation_calendar %>%
    purrr::pwalk(~create_collision_table(calendar, ...))

  get_event_type_colors() %>%
    purrr::pwalk(~apply_event_colors(pkg.env$wb, "Calendrier", ...))

  openxlsx::saveWorkbook(pkg.env$wb, file = path, overwrite = TRUE)
}

#' Cleanup calendar
#'
#' @param year An integer
#'
#' @return A tibble with the calendar
get_clean_calendar <- function(year) {
  get_my_fnch_events_excel(year) %>%
    dplyr::mutate(de = readr::parse_date(as.character(de)),
                  `à` = readr::parse_date(as.character(`à`))) %>%
    dplyr::mutate(Interval = lubridate::interval(de, `à`)) %>%
    dplyr::mutate(`Association régionale` = readr::parse_factor(`Association régionale`)) %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.character), stringr::str_trim)) %>%
    dplyr::arrange(de, `à`)
}

#' Style for event colors
#'
#' @return A tibble with type and color
get_event_type_colors <- function() {
  tibble::tribble(
    ~type,  ~color,
    "CA",   "#FABF8F",
    "CD",   "#95B3D7",
    "CC",   "#C4D79B",
    "CH",   "#BFBFBF",
    "CS",   "#DA9694",
    "CV",   "#B1A0C7"
  )
}

#' Apply style colors
#'
#' @param wb The workbook to write in
#' @param sheet The sheet to write in
#' @param ... The current row values
apply_event_colors <- function(wb, sheet, ...) {
  row <- tibble::tibble(...)
  openxlsx::conditionalFormatting(wb, sheet,
                                  cols = 1:10,
                                  rows = 1:5000,
                                  rule = glue::glue('$F1=="{row$type}"'),
                                  style = openxlsx::createStyle(bgFill = row$color)
  )
}

#' Create event table
#'
#' @param sunday A date
create_week_table <- function(calendar, sunday, sheet) {
  monday <- sunday - lubridate::days(6)
  week <- lubridate::interval(start = monday, end = sunday)

  selected_columns <- c("Lieu", "de", "à", "Canton", "Association régionale",
                        "Type", "Disciplines", "Epreuves prévues",
                        "Président CO", "Téléphone M")

  calendar %>%
    dplyr::filter(lubridate::int_overlaps(Interval, week)) %>%
    dplyr::arrange(Type, de, `à`) %>%
    dplyr::select(tidyselect::all_of(selected_columns))-> calendar_rows

  header <- glue::glue("Semaine {as.integer(format(sunday, '%W')) + 1} : {format(monday, '%d %b')} - {format(sunday, '%d %b')}")
  openxlsx::writeData(pkg.env$wb, sheet, tibble::tibble(header), startRow = pkg.env$start_row,
                      colNames = F, borders = "surrounding", borderStyle = "thick")
  pkg.env$start_row <- pkg.env$start_row + 1

  openxlsx::writeData(pkg.env$wb, sheet, calendar_rows, startRow = pkg.env$start_row,
                      colNames = F, borders = "surrounding", borderStyle = "thin")
  pkg.env$start_row <- pkg.env$start_row + nrow(calendar_rows) + 1
}

#' Create collision table
#'
#' @param ... The current row values
create_collision_table <- function(calendar, ...) {
  current <- tibble::tibble(...)

  selected_columns <- c("Lieu", "de", "à", "Canton", "Association régionale",
                        "Type", "Disciplines", "Epreuves prévues",
                        "Président CO", "Téléphone M")

  current %>%
    dplyr::select(tidyselect::all_of(selected_columns)) -> federation_row

  calendar %>%
    dplyr::filter(ID != current$ID) %>%
    dplyr::filter(lubridate::int_overlaps(Interval, current$Interval)) %>%
    dplyr::arrange(Type, de, `à`) %>%
    dplyr::select(tidyselect::all_of(selected_columns)) -> collision_rows

  openxlsx::writeData(pkg.env$wb, "Calendrier", federation_row, startRow = pkg.env$start_row,
                      colNames = F, borders = "surrounding", borderStyle = "thick")
  pkg.env$start_row <- pkg.env$start_row + 1

  openxlsx::writeData(pkg.env$wb, "Calendrier", collision_rows, startRow = pkg.env$start_row,
                      colNames = F, borders = "surrounding", borderStyle = "thin")
  pkg.env$start_row <- pkg.env$start_row + nrow(collision_rows) + 1
}

#' Create ICS Event for
#'
#' @param ... Must contain Start, End, Summary, Description and URL
#'
#' @return A calendar ICS event
create_ics_cal <- function(...) {
  event <- tibble::tibble(...)

  calendar::ic_event(start_time = event$Start,
                     end_time = event$End,
                     format = "%Y/%m/%d",
                     summary = event$Summary,
                     more_properties = TRUE,
                     event_properties = c("DESCRIPTION" = event$Description, "URL" = event$URL))
}
