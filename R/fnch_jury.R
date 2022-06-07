#' Get reactable Jury table
#'
#' @param tag An event tag
#' @param first_day Date of first class
#' @param last_day Date of last class
#'
#' @return A reactable
#' @export
fnch_reactable_jury <- function(tag, first_day, last_day) {
  judges <- fnch_get_judges()
  jury_data <- fnch_get_jury_data(tag, first_day, last_day)
  jury_dates <- fnch_get_jury_dates(jury_data, judges)

  row_details <- function(index) {
    date <- jury_dates[[index, "date"]]

    jury_day <- fnch_get_judge_table(jury_data, date)

    htmltools::div(
      class = "day-detail",
      jury_day |> reactable::reactable(
        class = "jury-table",
        defaultColDef = reactable::colDef(
          sortable = F,
          header = function(value) {
            if (value == "") return("") else fnch_get_class_data(jury_data, value)
          },
          align = "center"
        ),
        columns = list(
          juge = reactable::colDef(
            name = "",
            align = "left",
            minWidth = 220,
            maxWidth = 220,
            style = fnch_react_sticky_style(),
            headerStyle = fnch_react_sticky_style(),
            cell = function(value) {
              fnch_get_judge_data(judges, value)
            }
          )
        )
      )
    )
  }

  jury_dates |>
    reactable::reactable(
      columns = list(
        date = reactable::colDef(
          name = "Date",
          sortable = FALSE,
          cell = function(value) {
            withr::with_locale(
              new = c("LC_TIME" = "fr_CH"),
              code = htmltools::strong(strftime(value, format = "%a %d %B %Y"))
            )
          }
        ),
        nom = reactable::colDef(
          name = "PJ Responsable",
          sortable = FALSE
        )
      ),
      details = row_details,
      defaultColDef = reactable::colDef(headerClass = "header"),
      rowStyle = list(cursor = "pointer"),
      theme = reactable::reactableTheme(cellPadding = "2px 3px"),
      class = "plan-table"
    )
}

#' Get PDF Jury tables
#'
#' @param tag An event tag
#' @param first_day Date of first class
#' @param last_day Date of last class
#'
#' @return Knitr markdown
#' @export
fnch_pdftable_jury <- function(tag, first_day, last_day) {
  judges <- fnch_get_judges()
  jury_data <- fnch_get_jury_data(tag, first_day, last_day)
  jury_dates <- fnch_get_jury_dates(jury_data, judges)

  jury_dates |>
    purrr::pmap_chr(
      ~fnch_knit_judge_table(..1, ..2, jury_data)
    ) |> cat()
}

#' Knit jury for one day
#'
#' @param date Date
#' @param nom Name of the responsible jury president
#' @param df Jury data table
#'
#' @return A knitted string
fnch_knit_judge_table <- function(date, nom, df) {
  judges <- fnch_get_judges()
  jury_data <- df

  knitr::knit_child(input = rsvps::fnch_file("rmarkdown/templates/Jury/resources/Jury_day_child.Rmd"),
                    quiet = TRUE,
                    envir = environment())
}

#' Get FSSE Jury for given tag and dates
#'
#' @param tag An event tag
#' @param first_day Date of first class
#' @param last_day Date of last class
#'
#' @return A tibble
#' @keywords internal
fnch_get_jury_data <- function(tag, first_day, last_day) {
  judge_roles <- c("president", "juge", "paddock", "riviere", "reserve")
  rnotion::rni_get_database(
    id = "4565ac8fe140422ba9d6dec892c19623",
    filter = list(
      and = list(
        list(
          property = "Concours",
          select = list(
            equals = tag
          )
        ),
        list(
          property = "Date",
          date = list(
            on_or_after = first_day
          )
        ),
        list(
          property = "Date",
          date = list(
            on_or_before = last_day
          )
        )
      )
    ),
    sorts = list(
      list(
        property = "Ordre",
        direction = "ascending"
      )
    )
  ) |>
    tibble::tibble() |>
    dplyr::rename(data = 1) |>
    tidyr::hoist(
      data,
      id = list("id"),
      date = list("properties", "Date", "date", "start"),
      horaire = list("properties", "Horaire", "select", "name"),
      numero = list("properties", "Numéro", "rich_text", 1L, "plain_text"),
      nom = list("properties", "Nom", "title", 1L, "plain_text"),
      categorie = list("properties", "Catégorie", "select", "name"),
      categorie_col = list("properties", "Catégorie", "select", "color"),
      bareme = list("properties", "Barème", "select", "name"),
      bareme_col = list("properties", "Barème", "select", "color"),
      pj = list("properties", "PJ Responsable", "relation"),
      president = list("properties", "Président de l'épreuve", "relation"),
      juge = list("properties", "Juge aux points", "relation"),
      paddock = list("properties", "Juge au paddock", "relation"),
      riviere = list("properties", "Juge à la rivière", "relation"),
      reserve = list("properties", "Juge réserve", "relation")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(fnch_get_judge_functions(), "pj")),
        ~purrr::map(.x, fnch_map_judge_ids)
      )
    )
}

#' Get the table with date and responsible jury presidents
#'
#' @param jury_data A tibble with classes data
#' @param judges A tibble with judges data
#'
#' @return A tibble
#' @keywords internal
fnch_get_jury_dates <- function(jury_data, judges) {
  jury_data |>
    dplyr::select(date, pj) |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::mutate(pj = purrr::map(pj, ~fnch_get_judge_cols(.x, judges))) |>
    tidyr::unnest_wider(pj) |>
    dplyr::select(-role) |>
    dplyr::group_by(date) |>
    dplyr::summarise(nom = glue::glue_collapse(sort(nom), sep = ", ", last = " et "))
}

#' Get jury table for a specific date
#'
#' @param df Jury data table
#' @param filter_date Date to filter for
#'
#' @return A tibble
#' @export
fnch_get_judge_table <- function(df, filter_date) {
  judges <- fnch_get_judges()

  df |>
    tidyr::unnest(dplyr::all_of(fnch_get_judge_functions()), keep_empty = TRUE) |>
    tidyr::pivot_longer(
      dplyr::all_of(fnch_get_judge_functions()),
      names_to = "role", values_to = "juge"
    ) |>
    dplyr::mutate(role = fnch_get_judge_function_abbr(role)) |>
    dplyr::select(dplyr::all_of(c("id", "date", "role", "juge"))) |>
    dplyr::filter(juge != "NULL") |>
    dplyr::mutate(juge = as.character(juge)) |>
    dplyr::filter(date == filter_date) |>
    dplyr::select(-date) |>
    tidyr::pivot_wider(
      names_from = id, values_from = role,
      values_fn = dplyr::first
    ) |>
    dplyr::mutate(judges = purrr::map(juge, ~fnch_get_judge_cols(.x, judges))) |>
    tidyr::unnest_wider(judges) |>
    dplyr::mutate(role = fnch_role_order(role)) |>
    dplyr::arrange(role, nom) |>
    dplyr::select(-c(role, nom))
}

#' Change the judge role to a string "number" for sorting
#'
#' @param role Judge role
#'
#' @return A string
#' @keywords internal
fnch_role_order <- function(role) {
  dplyr::case_when(
    role == "PJ" ~ "1",
    role == "JN" ~ "2",
    role == "CJ" ~ "3",
    TRUE         ~ "4"
  )
}

#' Return judge data
#'
#' @param judge_id The judge id
#' @param df The judge data table
#'
#' @return A tibble
#' @export
fnch_get_judge_cols <- function(judge_id, df) {
  df |>
    dplyr::filter(id == judge_id) |>
    dplyr::select(nom, role)
}

#' Get data for a specific class
#'
#' @param df Jury data table
#' @param class_id Id of the class
#'
#' @return A list
#' @keywords internal
fnch_get_class_data <- function(df, class_id) {
  fnch_get_class_headers(class_id, df) |>
    as.list() -> data

  htmltools::tagList(
    htmltools::span(
      style = "cursor: pointer",
      htmltools::strong(data$numero)
    ),
    htmltools::span(class = "class_horaire", data$horaire),
    htmltools::span(
      class = "class_tag",
      style = paste0("background-color: ",
                     prismatic::color(data$categorie_col) |> prismatic::clr_alpha(.2),
                     ";"),
      data$categorie
    ),
    htmltools::span(
      class = "class_tag",
      style = paste0("background-color: ",
                     prismatic::color(data$bareme_col) |> prismatic::clr_alpha(.2),
                     ";"),
      data$bareme
    )
  )
}

#' Get data for a specific class
#'
#' @param df Jury data table
#' @param class_id Id of the class
#'
#' @return A datafrane
#' @export
fnch_get_class_headers <- function(class_id, df) {
  df |>
    dplyr::filter(id == class_id) |>
    dplyr::select(numero, nom, horaire, categorie, categorie_col, bareme, bareme_col)
}

#' Get FSSE Judges
#'
#' @return A tibble
#' @keywords internal
fnch_get_judges <- function() {
  rnotion::rni_get_database(id = "1117179c74094e198e291e48d24bd585") |>
    tibble::tibble() |>
    dplyr::rename(data = 1) |>
    tidyr::hoist(
      data,
      id = c("id"),
      nom = list("properties", "Name", "title", 1L, "plain_text"),
      role = list("properties", "Rôle", "select", "name")
    )
}

#' Get data for a specific judge
#'
#' @param df Judge data table
#' @param judge_id Id of the judge
#'
#' @return A list
#' @keywords internal
fnch_get_judge_data <- function(df, judge_id) {
  df |>
    dplyr::filter(id == judge_id) |>
    dplyr::select(nom, role) |>
    as.list() -> data

  htmltools::tagList(
    htmltools::span(class = "judge_role", data$role),
    htmltools::strong(data$nom)
  )
}

#' Get judge ids
#'
#' @param data A list of notion objects
#'
#' @return A list of ids
#' @keywords internal
fnch_map_judge_ids <- function(data) {
  data |>
    purrr::map(
      ~purrr::pluck(.x, "id")
    )
}

#' Get judge functions
#'
#' @return A vector of judge roles
#' @keywords internal
fnch_get_judge_functions <- function() {
  c("president", "juge", "paddock", "riviere", "reserve")
}

#' Get judge role abbreviation
#'
#' @param role Judge role
#'
#' @keywords internal
fnch_get_judge_function_abbr <- function(role) {
  dplyr::case_when(
    role == "president" ~ "P",
    role == "juge"      ~ "J",
    role == "paddock"   ~ "A",
    role == "riviere"   ~ "RI",
    role == "reserve"   ~ "RE",
    TRUE                ~ "RE"
  )
}

#' Helper function for sticky columns
#'
#' @param left Boolean: align left?
#'
#' @keywords internal
fnch_react_sticky_style <- function(left = TRUE) {
  style <- list(position = "sticky", background = "#fff", zIndex = 1)
  if (left) {
    style <- c(style, list(left = 0, borderRight = "1px solid #eee"))
  } else {
    style <- c(style, list(right = 0, borderLeft = "1px solid #eee"))
  }
  style
}
