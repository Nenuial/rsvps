#' Get Events for speaker selection
#'
#' @return A tibble
#' @export
get_fnch_logbook_events <- function() {
  today <- clock::date_today(zone = "Europe/Zurich")

  today |>
    clock::add_weeks(-1) |>
    get_fnch_events(
      format(clock::date_today(zone = "Europe/Zurich"), "%Y-12-31"),
      disziplin = "SP"
    ) |>
    dplyr::filter(hat_startlisten, status == "geplant")
}

#' Insert a log event into notion
#'
#' @param event_name The event location
#' @param event_judge The judge id
#' @param event_horse_id A numeric passeport id
#' @param event_horse The Horse name
#' @param event_rider_id A numeric license id
#' @param event_rider The rider name
#' @param event_categories  A vector of categories
#' @param event_comment A comment
#'
#' @export
fnch_add_logbook_event <- function(
    event_name,
    event_judge,
    event_horse_id, event_horse,
    event_rider_id, event_rider,
    event_categories,
    event_comment
) {

  event_categories |>
    purrr::map(rnotion::rni_map_multi_select_list) -> event_categories_list

  rnotion::rni_add_page(
    parent = list(
      type = "database_id",
      database_id = "981e78552f3c4eee9b5761d1a73754b8"
    ),

    properties = list(
      # The event
      Concours = list(
        select = list(
          name = event_name
        )
      ),

      # The judge
      Juge = list(
        relation = list(
          list(
            id = event_judge
          )
        )
      ),

      # The event timestamp
      Timestamp = list(
        date = list(
          start = clock::date_now(zone = "Europe/Zurich") |> as.character(),
          time_zone = "Europe/Zurich"
        )
      ),

      # The horse infos
      Cheval = list(
        rich_text = list(
          list(
            text = list(
              content = event_horse
            )
          )
        )
      ),

      Passeport = list(
        number = event_horse_id
      ),

      # The rider infos

      Cavalier = list(
        title = list(
          list(
            text = list(
              content = event_rider
            )
          )
        )
      ),

      Licence = list(
        number = event_rider_id
      ),

      # The event classification
      `Catégorie` = list(
        multi_select = event_categories_list
      ),

      # The comment
      Commentaire = list(
        rich_text = list(
          list(
            text = list(
              content = event_comment
            )
          )
        )
      )
    )
  )
}
