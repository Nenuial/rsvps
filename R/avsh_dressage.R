#' Get AVSH Championship ranking list
#'
#' @param df A results dataframe
#' @param res Number of results to consider
#' @param lic Licenses to consider (vector)
#' @param ep_selection Categories to consider
#' @param kur Number of Kürs to consider
#'
#' @return A cleaned up results dataframe
#' @export
get_avsh_championship_ranking <- function(df, res, lic, ep_selection, kur = 0) {
  args <- return_fer_ranking_arguments(res)

  df %<>%
    dplyr::filter(str_detect(lizenzen, !!lic),
                  percent >= 60)

  if (kur > 0) {
    df %>%
      dplyr::filter(kategorie_code %in% get_fnch_dr_kur_levels()) %>%
      dplyr::group_by(reiter_id, pferde_id, reiter_name, reiter_ort, pferde_name, punkte_total) %>%
      dplyr::arrange(-percent) %>%
      dplyr::slice(1:kur) %>%
      dplyr::ungroup() -> df_kur

    df %>%
      dplyr::filter(kategorie_code %in% ep_selection) -> df_res

    df <- rbind(df_res, df_kur)

    ep_selection <- c(ep_selection, get_fnch_dr_kur_levels())
  }

  df %<>%
    dplyr::filter(str_detect(lizenzen, !!lic),
                  kategorie_code %in% !!ep_selection,
                  percent >= 60) %>%
    dplyr::group_by(reiter_id, pferde_id, reiter_name, reiter_ort, pferde_name, punkte_total) %>%
    dplyr::arrange(-percent) %>%
    dplyr::slice(1:res) %>%
    dplyr::summarise(!!!args) %>%
    dplyr::arrange(-count, -moy)

  return(df)
}

#' Get AVSH B Dressage classes
#'
#' @return A vector of strings
#' @export
get_avsh_championship_b_classes <- function() {
  return(pkg.env$ep_ch_fb)
}

#' Get AVSH R Dressage classes
#'
#' @return A vector of strings
#' @export
get_avsh_championship_r_classes <- function() {
  ep <- c(pkg.env$ep_ch_l, pkg.env$ep_ch_m)

  return(ep)
}

#' Get AVSH N Dressage classes
#'
#' @return A vector of strings
#' @export
get_avsh_championship_n_classes <- function() {
  ep <- c(pkg.env$ep_ch_m, pkg.env$ep_ch_s)

  return(ep)
}

#' Get AVSH M Dressage classes
#'
#' @return A vector of strings
#' @export
get_avsh_championship_m_classes <- function() {
  ep <- c(pkg.env$ep_ch_m)

  return(ep)
}

#' Get AVSH S Dressage classes
#'
#' @return A vector of strings
#' @export
get_avsh_championship_s_classes <- function() {
  ep <- c(pkg.env$ep_ch_s)

  return(ep)
}
