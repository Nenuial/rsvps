#' Results table for dressage classes
#'
#' @param file The results file
#' @param provisional Boolean: is the result provisional?
#'
#' @return A reactable object
#' @export
event_dr_results <- function(file, provisional = FALSE) {
  event_dr_read_result_file(file) |>
    dplyr::select(Rang = Rang...12, Cavalier = Reiter, Cheval = `Name Pferd`,
                  matches("^[HCM]$"),
                  Total = `Total Punkte`, `%` = Gesamttotal) -> results

  last_rider <- ""

  if(provisional) {
    last_rider <- results[[nrow(results), "Cheval"]]
  }

  results |>
    dplyr::arrange(Rang) |>
    event_dr_results_table(last_rider)
}

#' Results table for dressage championship classes
#'
#' @param file_round1 The result file of the first round
#' @param file_round2 The result file of the second round
#' @param provisional Boolean: is the result provisional?
#'
#' @return A reactable object
#' @export
event_dr_results_championship <- function(file_round1, file_round2, provisional = FALSE) {
  event_dr_read_result_file(file_round1) |>
    dplyr::select(Cavalier = Reiter, Cheval = `Name Pferd`, `%1` = Gesamttotal) -> results_round_1

  event_dr_read_result_file(file_round2) |>
    dplyr::select(Cavalier = Reiter,
                  Cheval = `Name Pferd`,
                  matches("^[HCM]$"), `%2` = Gesamttotal) -> results_round_2

  last_rider <- ""

  if(provisional) {
    last_rider <- results_round_2[[nrow(results_round_2), "Cheval"]]
  }

  results_round_1 |>
    dplyr::left_join(results_round_2, by = c("Cavalier", "Cheval")) |>
    dplyr::mutate(`%` = round((`%1` + `%2`) / 2, 2)) |>
    dplyr::arrange(desc(`%`), desc(`%2`)) |>
    dplyr::mutate(Rang = rank((tidyr::replace_na(`%2`, 0) + `%1`)*-1, ties.method = "min"),
                  .before = "Cavalier") -> results


  results |>
    event_dr_results_table(last_rider, championship = TRUE)
}

#' Results table for dressage intercantonal individual
#'
#' @param file_round1 The result file of the first round
#' @param file_round2 The result file of the second round
#' @param affiliation File with the canton affiliations
#' @param provisional Boolean: is the result provisional?
#'
#' @return A reactable object
#' @export
event_dr_results_intercantonal_individual <- function(file_round1, file_round2, affiliation, provisional = FALSE) {
  event_dr_read_result_file(file_round1) |>
    dplyr::select(Cavalier = Reiter, Cheval = `Name Pferd`, Licence = `Lizenz Nr`, `%1` = Gesamttotal) -> results_round_1

  event_dr_read_result_file(file_round2) |>
    dplyr::select(Cavalier = Reiter,
                  Cheval = `Name Pferd`, `%2` = Gesamttotal) -> results_round_2

  last_rider <- ""

  if(provisional) {
    last_rider <- results_round_2[[nrow(results_round_2), "Cheval"]]
  }

  results_round_1 |>
    dplyr::left_join(results_round_2, by = c("Cavalier", "Cheval")) |>
    dplyr::mutate(`%` = round((`%1` + `%2`) / 2, 2)) |>
    dplyr::arrange(desc(`%`), desc(`%2`)) |>
    dplyr::left_join(
      readxl::read_excel(affiliation) |> dplyr::select(Licence, Canton, Programme),
      by = "Licence"
    ) |>
    dplyr::select(-Licence) |>
    dplyr::relocate(c(Programme, Canton), .before = "Cavalier") |>
    dplyr::group_by(Programme) |>
    dplyr::mutate(Rang = rank((tidyr::replace_na(`%2`, 0) + `%1`)*-1, ties.method = "min"),
                  .before = "Programme") |>
    dplyr::ungroup() -> results


  results |>
    event_dr_results_table(last_rider, intercantonal = TRUE, individual = TRUE)
}

#' Results table for dressage classes with canton flag
#'
#' @param file The results file
#' @param affiliation File with the canton affiliations
#' @param provisional Boolean: is the result provisional?
#'
#' @return A reactable object
#' @export
event_dr_results_intercantonal <- function(file, affiliation, provisional = FALSE) {
  event_dr_read_result_file(file) |>
    dplyr::select(Rang = Rang...12, Cavalier = Reiter, Cheval = `Name Pferd`,
                  matches("^[HCM]$"), Licence = `Lizenz Nr`,
                  Total = `Total Punkte`, `%` = Gesamttotal) |>
    dplyr::left_join(
      readxl::read_excel(affiliation) |> dplyr::select(Licence, Canton),
      by = "Licence"
    ) |>
    dplyr::select(-Licence) |>
    dplyr::relocate(Canton, .before = "Cavalier") -> results

  last_rider <- ""

  if(provisional) {
    last_rider <- results[[nrow(results), "Cheval"]]
  }

  results |>
    dplyr::arrange(Rang) |>
    event_dr_results_table(last_rider, intercantonal = T)
}


#' Results table for dressage classes with canton flag
#'
#' @param file_round1 The result file of the first round
#' @param file_round2 The result file of the second round
#' @param affiliation File with the canton affiliations
#'
#' @return A reactable object
#' @export
event_dr_results_intercantonal_final <- function(file_round1, file_round2 = "", affiliation) {
  event_dr_read_result_file(file_round1) |>
    dplyr::select(Cavalier = Reiter, Cheval = `Name Pferd`, `%` = Gesamttotal, Licence = `Lizenz Nr`) |>
    dplyr::mutate(Licence = as.numeric(Licence), `%` = as.numeric(`%`)) |>
    dplyr::left_join(
      readxl::read_excel(affiliation) |> dplyr::select(Licence, Canton),
      by = "Licence"
    ) |>
    dplyr::select(-Licence) |>
    dplyr::group_by(Canton) |>
    dplyr::arrange(dplyr::desc(`%`), .by_group = TRUE) |>
    dplyr::slice(1:3) |>
    dplyr::ungroup() |>
    dplyr::mutate(`Épreuve` = "Manche 1", .after = "Cheval") -> results

  if(file_round2 != "") {
    event_dr_read_result_file(file_round2) |>
      dplyr::select(Cavalier = Reiter, Cheval = `Name Pferd`, `%` = Gesamttotal, Licence = `Lizenz Nr`) |>
      dplyr::mutate(Licence = as.numeric(Licence), `%` = as.numeric(`%`)) |>
      dplyr::left_join(
        readxl::read_excel(affiliation) |> dplyr::select(Licence, Canton),
        by = "Licence"
      ) |>
      dplyr::select(-Licence) |>
      dplyr::group_by(Canton) |>
      dplyr::arrange(dplyr::desc(`%`), .by_group = TRUE) |>
      dplyr::slice(1:3) |>
      dplyr::ungroup() |>
      dplyr::mutate(`Épreuve` = "Manche 2", .after = "Cheval") -> results_round_2

    results |>
      dplyr::bind_rows(results_round_2) -> results
  }

  last_rider <- ""

  results |>
    dplyr::relocate(Canton, .before = "Cavalier") |>
    event_dr_results_table(last_rider, championship = T, intercantonal = T)
}


#' Format the results in a reactable table
#'
#' @param results The results dataframe
#' @param last_rider The last rider (if highlighting is desired)
#' @param championship Boolean for championship tables
#'
#' @return A reactable object
#' @keywords internal
event_dr_results_table <- function(results, last_rider, championship = FALSE, intercantonal = FALSE, individual = FALSE) {
  results |>
    names() |>
    stringr::str_extract("^[HCM]$") |>
    na.omit() -> judges_cols

  default_sorted <- "Rang"
  default_sort_order <- "asc"
  column_group_by <- NULL

  column_list <- list(
    Rang = reactable::colDef(
      width = 80,
      format = reactable::colFormat(digits = NULL)
    ),
    Cavalier = reactable::colDef(
      width = 320
    ),
    Cheval = reactable::colDef(
      width = 400
    ),
    `%` = reactable::colDef(
      width = 80,
      format = reactable::colFormat(digits = 2, suffix = " %")
    )
  )

  column_groups <- list(
    reactable::colGroup(name = "Juge", columns = judges_cols),
    reactable::colGroup(name = "Résultat", columns = c("Total", "%"))
  )

  if(championship) {
    column_list <- list(
      Rang = reactable::colDef(
        format = reactable::colFormat(digits = NULL)
      ),
      `%1` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      ),
      `%2` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      ),
      `%` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      )
    )

    column_groups <- list(
      reactable::colGroup(name = "Manche 1", columns = c("%1")),
      reactable::colGroup(name = "Manche 2", columns = c(judges_cols, "%2")),
      reactable::colGroup(name = "Finale", columns = c("%"))
    )
  }

  if(intercantonal) {
    column_list <- list(
      Canton = reactable::colDef(cell = function(value) {
        image <- htmltools::img(src = event_canton_flag(value), height = "24px")
        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block", width = "45px"), image)
        )
      }),
      Rang = reactable::colDef(
        format = reactable::colFormat(digits = NULL)
      ),
      `%` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      )
    )
  }

  if(intercantonal & championship) {
    column_list <- list(
      Canton = reactable::colDef(
        cell = function(value) {
          image <- htmltools::img(src = event_canton_flag(value), height = "24px")
          htmltools::tagList(
            htmltools::div(style = list(display = "inline-block", width = "45px"), image)
          )
        },
        grouped = reactable::JS("function(cellInfo) {
              switch (cellInfo.value) {
                case 'FR':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_Canton_of_Fribourg.svg/240px-Flag_of_Canton_of_Fribourg.svg.png\" />';
                  break;
                case 'GE':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_Canton_of_Geneva.svg/240px-Flag_of_Canton_of_Geneva.svg.png\" />';
                  break;
                case 'JU':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Flag_of_Canton_of_Jura.svg/239px-Flag_of_Canton_of_Jura.svg.png\" />';
                  break;
                case 'NE':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Flag_of_Canton_of_Neuchâtel.svg/240px-Flag_of_Canton_of_Neuchâtel.svg.png\" />';
                  break;
                case 'TI':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Canton_of_Tessin.svg/240px-Flag_of_Canton_of_Tessin.svg.png\" />';
                  break;
                case 'VD':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Flag_of_Canton_of_Vaud.svg/239px-Flag_of_Canton_of_Vaud.svg.png\" />';
                  break;
                case 'VS':
                  imgValue = '<img width=\"45\" src=\"https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_Canton_of_Valais.svg/239px-Flag_of_Canton_of_Valais.svg.png\" />';
                  break;
                default:
                  imgValue =  '';
              }

              return imgValue;
            }"),
        html = TRUE
      ),
      `%` = reactable::colDef(
        aggregate = "mean",
        format = reactable::colFormat(digits = 2, suffix = " %")
      )
    )

    column_group_by <- "Canton"
    column_groups <- NULL
    default_sorted <- "%"
    default_sort_order <- "desc"
  }

  if(intercantonal & individual) {
    column_list <- list(
      Canton = reactable::colDef(cell = function(value) {
        image <- htmltools::img(src = event_canton_flag(value), height = "24px")
        htmltools::tagList(
          htmltools::div(style = list(display = "inline-block", width = "45px"), image)
        )
      }),
      Rang = reactable::colDef(
        format = reactable::colFormat(digits = NULL)
      ),
      `%1` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      ),
      `%2` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      ),
      `%` = reactable::colDef(
        format = reactable::colFormat(digits = 2, suffix = " %")
      )
    )

    column_group_by <- "Programme"
    column_groups <- NULL
  }

  results |>
    reactable::reactable(
      pagination = FALSE,
      wrap = FALSE,
      defaultSorted = default_sorted,
      defaultSortOrder = default_sort_order,
      defaultExpanded = TRUE,
      defaultColGroup = reactable::colGroup(headerClass = "group-header"),
      defaultColDef = reactable::colDef(
        class = "cell", headerClass = "header", width = 80,
        format = reactable::colFormat(digits = 1)
      ),
      groupBy = column_group_by,
      columnGroups = column_groups,
      columns = column_list,
      showSortable = TRUE,
      rowStyle = function(index) {
        if (results[[index, "Cheval"]] == last_rider) list(background = "rgba(46, 144, 147, 0.3)")
      },
      theme = reactable::reactableTheme(
        backgroundColor = "#1fe0",
        stripedColor = "#1fe0",
        highlightColor = "#1fe0",
        style = list(
          background = "#1fe0"
        ),
        groupHeaderStyle = list(
          background = "#1fe0"
        )
      )
    )
}

#' Read the Rosson dressage results
#'
#' Reads a Rosson result file for a dressage class
#' and renames judge columns to just the letter.
#'
#' @param path A file path
#'
#' @return A dataframe
#' @keywords internal
event_dr_read_result_file <- function(path) {
  readr::read_tsv(path, locale = readr::locale(encoding = "ISO-8859-1")) |>
    dplyr::rename_with(~ stringr::str_replace(.x, pattern = "Punkte \\(richter ([HCM])\\)", replacement = "\\1"))
}

#' Return flag url
#'
#' @param id Canton short id
#'
#' @return A string url
#' @keywords internal
event_canton_flag <- function(id) {
  dplyr::case_when(
    id == "FR" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/Flag_of_Canton_of_Fribourg.svg/240px-Flag_of_Canton_of_Fribourg.svg.png",
    id == "GE" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_Canton_of_Geneva.svg/240px-Flag_of_Canton_of_Geneva.svg.png",
    id == "JU" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/Flag_of_Canton_of_Jura.svg/239px-Flag_of_Canton_of_Jura.svg.png",
    id == "NE" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Flag_of_Canton_of_Neuchâtel.svg/240px-Flag_of_Canton_of_Neuchâtel.svg.png",
    id == "TI" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/1/11/Flag_of_Canton_of_Tessin.svg/240px-Flag_of_Canton_of_Tessin.svg.png",
    id == "VD" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/Flag_of_Canton_of_Vaud.svg/239px-Flag_of_Canton_of_Vaud.svg.png",
    id == "VS" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Flag_of_Canton_of_Valais.svg/239px-Flag_of_Canton_of_Valais.svg.png"
  )
}
