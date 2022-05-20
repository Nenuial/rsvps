#' FSSE Accounting: Sum up and format
#'
#' @param tbl An FSSE Accounting tibble
#'
#' @return A flextable
#' @export
fnch_accounting <- function(tbl) {
  total <- sum(tbl$`Défraiement`)

  tbl |>
    flextable::flextable() |>
    flextable::colformat_num(
      j = ncol(tbl), prefix = "CHF ", suffix = ".-"
    ) |>
    flextable::add_footer_row(values = paste0("CHF ", total, ".-"), colwidths = ncol(tbl)) |>
    flextable::theme_alafoli() |>
    flextable::font(fontname = "Fira Sans Light") |>
    flextable::hline_bottom(
      j = ncol(tbl),
      border = officer::fp_border(color = "grey", style = "solid", width = 1),
      part = "body"
    ) |>
    flextable::align(align = "right", part = "footer") -> flex

  if (ncol(tbl) == 3) {
    flex |>
      flextable::width(j = c(1), width = 8.9, unit = "cm") |>
      flextable::width(j = c(2,3), width = 3.8, unit = "cm") -> flex
  } else if (ncol(tbl) == 4) {
    flex |>
      flextable::width(j = c(1), width = 5, unit = "cm") |>
      flextable::width(j = c(2:4), width = 3.8, unit = "cm") -> flex
  }

  flex
}

#' FSSE Accounting: Base presidency table
#'
#' @return A tibble
#' @export
fnch_presidency_base <- function() {
  tibble::tibble(
    " " = character(),
    "Jours" = numeric()
  )
}

#' FSSE Accounting: Add row to presidency table
#'
#' @param df A presidency table
#' @param name Name of president
#' @param days Number of days
#'
#' @return A tibble
#' @export
fnch_add_presidency <- function(df, name, days) {
  df |>
    tibble::add_row(
      " " = name,
      "Jours" = days
    )
}

#' FSSE Accounting: Sum up presidency
#'
#' @param df A presidency table
#'
#' @return A tibble
#' @export
fnch_sum_presidency <- function(df) {
  df |>
    dplyr::mutate("Défraiement" = Jours * 100)
}

#' FSSE Accounting: Base juge table
#'
#' @return A tibble
#' @export
fnch_judges_base <- function() {
  tibble::tibble(
    " " = character(),
    "½ Jours" = numeric(),
    "Jours" = numeric()
  )
}

#' FSSE Accounting: Add row to judges table
#'
#' @param df A judge table
#' @param name Name of judge
#' @param half_days Number of half days judged
#' @param days Number of days judged
#'
#' @return A tibble
#' @export
fnch_add_judge <- function(df, name, half_days, days) {
  df |>
    tibble::add_row(
      " " = name,
      "½ Jours" = half_days,
      "Jours" = days
    )
}

#' FSSE Accounting: Sum up judges
#'
#' @param df A judge table
#'
#' @return A tibble
#' @export
fnch_sum_judges <- function(df) {
  df |>
    dplyr::mutate("Défraiement" = `½ Jours` * 120 + Jours * 200)
}

#' FSSE Accounting: Base juge table (old system)
#'
#' @return A tibble
#' @export
fnch_judges_old_base <- function() {
  tibble::tibble(
    " " = character(),
    "Épreuves" = numeric()
  )
}

#' FSSE Accounting: Add row to judges table (old system)
#'
#' @param df A judges table (old system)
#' @param name Name of judge
#' @param classes Number of classes
#'
#' @return A tibble
#' @export
fnch_add_old_judge <- function(df, name, classes) {
  df |>
    tibble::add_row(
      " " = name,
      "Épreuves" = classes
    )
}

#' FSSE Accounting: Sum up judges (old system)
#'
#' @param df A judges table (old system)
#'
#' @return A tibble
#' @export
fnch_sum_old_judges <- function(df, tarif = 30) {
  df |>
    dplyr::mutate("Défraiement" = `Épreuves` * tarif)
}

#' FSSE Jury: event table
#'
#' @param nrow Number of rows
#'
#' @return A flextable
#' @export
fnch_main_courante <- function(nrow = 20) {
  black_border = officer::fp_border(color="black", width = .5)

  tibble::tibble(
    Date = rep(" ", nrow),
    Heure = rep(" ", nrow),
    Licence = rep(" ", nrow),
    `Événement` = rep(" ", nrow)
  ) |>
    flextable::flextable() |>
    flextable::width(j = c(1:2), width = 2.4, unit = "cm") |>
    flextable::width(j = 3, width = 2.8, unit = "cm") |>
    flextable::width(j = 4, width = 8.5, unit = "cm") |>
    flextable::theme_box() |>
    flextable::font(fontname = "Fira Sans Light") |>
    flextable::height_all(1.5, unit = "cm") |>
    flextable::border_remove() |>
    flextable::border_outer(part="all", border = black_border) |>
    flextable::border_inner_h(part="all", border = black_border) |>
    flextable::border_inner_v(part="all", border = black_border)
}

