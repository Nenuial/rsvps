#' Title
#'
#' @param df A results dataframe
#' @param res Number of results to consider
#' @param lic Licenses to consider (vector)
#' @param ep_selection Categories to consider
#' @param kur Number of Kürs to consider
#'
#' @return A cleaned up results dataframe
#' @export
get_fer_championship_ranking <- function(df, res, lic, ep_selection, kur = 0) {
  args <- return_fer_ranking_arguments(res)

  df %<>%
    dplyr::filter(str_detect(LicenceTyp, !!lic),
                  percent >= 60)

  if(kur > 0) {
    kurlist <- c("LK", "MK", "GEORGK")

    df %>%
      dplyr::filter(kategorie_code %in% kurlist) %>%
      dplyr::group_by(reiter_id, pferde_id, reiter_name, ZIP, reiter_ort, pferde_name, punkte_total) %>%
      dplyr::arrange(-percent) %>%
      dplyr::slice(1:kur) %>%
      dplyr::ungroup() -> df_kur

    df %>%
      dplyr::filter(kategorie_code %in% ep_selection) -> df_res

    df <- rbind(df_res, df_kur)

    ep_selection <- c(ep_selection, kurlist)
  }

  df %<>%
    dplyr::filter(str_detect(LicenceTyp, !!lic),
                  kategorie_code %in% !!ep_selection,
                  percent >= 60) %>%
    dplyr::group_by(reiter_id, pferde_id, reiter_name, ZIP, reiter_ort, pferde_name, punkte_total) %>%
    dplyr::arrange(-percent) %>%
    dplyr::slice(1:res) %>%
    dplyr::summarise(!!!args) %>%
    dplyr::filter(count >= res) %>%
    dplyr::arrange(-moy)

  return(df)
}

#' Generate arguments
#'
#' @param x Number of arguments for ranking list to generate
#'
#' @return List of arguments
return_fer_ranking_arguments <- function(x) {
  args <- purrr::map(1:x, map_ranking_arguments) %>% unlist()

  args[["count"]] <- rlang::quo(n())
  args[["moy"]] <- rlang::quo(mean(percent))

  return(args)
}

#' Function to build arguments for ranking list
#'
#' @param i Number of arguments
#'
#' @return A list of arguments
map_ranking_arguments <- function(i) {
  lnames <- c(glue::glue("per{i}"),
              glue::glue("cat{i}"),
              glue::glue("lieu{i}"))
  dl <- list(rlang::quo(dplyr::nth(percent, !!i)),
             rlang::quo(dplyr::nth(kategorie_code, !!i)),
             rlang::quo(dplyr::nth(event_ort, !!i))) %>% stats::setNames(lnames)

  return(dl)
}

#' Determine level for U21 champioship
#'
#' @param df A results dataframe
#'
#' @return A results dataframe
#' @export
add_category_u21 <- function(df) {
  df %>%
    dplyr::mutate(championnat = dplyr::case_when(nb_ep_fb >= 1 & nb_ep_l < 2 & punkte_total <= 160 ~ "FB",
                                                 nb_ep_l >= 1 & nb_ep_m < 2 & punkte_total <= 1500 ~ "L",
                                                 nb_ep_m >= 1 & nb_ep_s < 2 & punkte_total <= 3500 ~ "M",
                                                 nb_ep_s >= 1                                      ~ "S",
                                                 TRUE                                              ~ "??"))
}

#' Determine level for R champioship
#'
#' @param df A results dataframe
#' @param x Number of categories to consider
#' @param nb_cat Max number of M results
#' @param max_pt Max number of points for L level
#'
#' @return A results dataframe
#' @export
add_category_r <- function(df, x, nb_cat, max_pt) {
  args <- return_fer_category_arguments(x)

  df %>%
    dplyr::mutate(championnat = ifelse(sum(c(!!!args) %in% c(ep_ch_m, "MK")) > nb_cat | punkte_total >= max_pt, "M", "L"))
}

#' Determine level for N champioship
#'
#' @param df A results dataframe
#' @param x Number of categories to consider
#' @param nb_cat Max number of S results
#' @param max_pt Max number of points for M level
#'
#' @return A results dataframe
#' @export
add_category_n <- function(df, x, nb_cat, max_pt) {
  args <- return_fer_category_arguments(x)

  df %>%
    dplyr::mutate(championnat = ifelse(sum(c(!!!args) %in% c(ep_ch_s, "GEORGK")) > nb_cat | punkte_total >= max_pt | nb_ep_s > 2, "S", "M"))
}

#' Generate category arguments
#'
#' @param x Number of arguments to generate for category
#'
#' @return List of category arguments
return_fer_category_arguments <- function(x) {
  args <- purrr::map(1:x, map_category_arguments) %>% unlist()

  return(args)
}

#' Function to build arguments for category list
#'
#' @param i Number of arguments to generate for category
#'
#' @return
map_category_arguments <- function(i) {
  arg <- rlang::sym(glue::glue("cat{i}"))

  return(rlang::quo(!!arg))
}

#' Rename the columns of the fer championship ranking
#'
#' @param df A dataframe
#'
#' @return A dataframe
#' @export
rename_ranking_columns <- function(df) {
  df %>%
    dplyr::rename(!!! rename_matrix[rename_matrix %in% colnames(.)]) %>%
    dplyr::rename_at(dplyr::vars(starts_with("lieu")),
                     dplyr::funs(str_replace(., "lieu", "Conc. "))) %>%
    dplyr::rename_at(dplyr::vars(starts_with("per")),
                     dplyr::funs(str_replace(., "per", "% "))) %>%
    dplyr::rename_at(dplyr::vars(starts_with("cat")),
                     dplyr::funs(str_replace(., "cat", "Ep. ")))
}

#' Calculate mean grade from judges' note
#'
#' @param obj A result object
#'
#' @return A numeric mean grade
#' @export
calculate_judges_mean <- function(obj) {
  avg <- round(mean(as.numeric(obj$prozent)), digits = 2)

  return(avg)
}
