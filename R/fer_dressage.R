#' Get FER Championship ranking list
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
    dplyr::filter(str_detect(reiter_lizenzen, !!lic),
                  percent >= 60)

  if (kur > 0) {
    df %>%
      dplyr::filter(resultate_kategorie_code %in% get_fnch_dr_kur_levels()) %>%
      dplyr::group_by(resultate_reiter_id, resultate_pferde_id, resultate_reiter_name, resultate_reiter_ort,
                      resultate_pferde_name, punkte_total) %>%
      dplyr::arrange(-percent) %>%
      dplyr::slice(1:kur) %>%
      dplyr::ungroup() -> df_kur

    df %>%
      dplyr::filter(resultate_kategorie_code %in% ep_selection) -> df_res

    df <- rbind(df_res, df_kur)

    ep_selection <- c(ep_selection, get_fnch_dr_kur_levels())
  }

  df %<>%
    dplyr::filter(str_detect(reiter_lizenzen, !!lic),
                  resultate_kategorie_code %in% !!ep_selection,
                  percent >= 60) %>%
    dplyr::group_by(resultate_reiter_id, resultate_pferde_id, resultate_reiter_name, resultate_reiter_ort,
                    resultate_pferde_name, punkte_total) %>%
    dplyr::arrange(-percent) %>%
    dplyr::slice(1:res) %>%
    dplyr::summarise(!!!args) %>%
    dplyr::filter(count >= res) %>%
    dplyr::arrange(-moy)

  return(df)
}

#' Get FER Swiss R Championship ranking list
#'
#' @param df A results dataframe
#' @param res Number of results to consider
#' @param lic Licenses to consider (vector)
#' @param ep_selection Categories to consider
#' @param kur Number of Kürs to consider
#'
#' @return A cleaned up results dataframe
#' @export
get_fer_championship_swiss_r_ranking <- function(df, ep_selection, res = 4, lic = "DR", nb_sel = 1) {
  args <- return_fer_ranking_arguments(res)

  if (nb_sel > 0) {
    nb_off_sel <- res - nb_sel
    df %>%
      dplyr::filter(!(resultate_kategorie_code %in% ep_selection)) %>%
      dplyr::group_by(reiter_id, pferde_id, reiter_name, reiter_ort, pferde_name, punkte_total) %>%
      dplyr::arrange(-percent) %>%
      dplyr::slice(1:nb_off_sel) %>%
      dplyr::ungroup() -> df_nb_sel

    df %>%
      dplyr::filter(resultate_kategorie_code %in% ep_selection) -> df_res

    df <- rbind(df_res, df_nb_sel)
  }

  df %<>%
    dplyr::filter(str_detect(lizenzen, !!lic)) %>%
    dplyr::group_by(reiter_id, pferde_id, reiter_name, reiter_ort, pferde_name, punkte_total) %>%
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
             rlang::quo(dplyr::nth(resultate_kategorie_code, !!i)),
             rlang::quo(dplyr::nth(ort, !!i))) %>% stats::setNames(lnames)

  return(dl)
}

#' Determine level for U21 championship
#'
#' @param df A results dataframe
#'
#' @return A results dataframe
#' @export
add_category_u21 <- function(df) {
  df %>%
    dplyr::mutate(championnat = dplyr::case_when(part_fb >= 1 & nb_ep_l < 4 & punkte_total <= 160  ~ "FB",
                                                 part_l  >= 1 & nb_ep_m < 4 & punkte_total <= 1500 ~ "L",
                                                 part_m  >= 1 & part_s < 1  & punkte_total <= 3000 ~ "M",
                                                 part_s  >= 1                                      ~ "S",
                                                 TRUE                                              ~ "??"))
}

#' Determine level for R championship
#'
#' @param df A results dataframe
#' @param nb_m Max number of M results
#' @param max_pt Max number of points for L level
#'
#' @return A results dataframe
#' @export
add_category_r <- function(df, nb_m, max_pt) {
  df %>%
    dplyr::mutate(championnat = dplyr::case_when(nb_ep_m > nb_m | punkte_total > max_pt ~ "M",
                                                 TRUE                                   ~ "L"))
}

#' Determine level for N championship
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
    dplyr::mutate(championnat = ifelse(sum(c(!!!args) %in% c(get_fnch_dr_s_levels(), "GEORGK"), na.rm = T) > nb_cat | punkte_total >= max_pt | nb_ep_s > 2, "S", "M"))
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
#' @return Quoted sym cat argument
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
  rename_matrix <- c('Lic.' = 'resultate_reiter_id',
                     'Pass.' = 'resultate_pferde_id',
                     'Nom' = 'resultate_reiter_name',
                     'Lieu' = 'resultate_reiter_ort',
                     'Cheval' = 'resultate_pferde_name',
                     'SoP' = 'punkte_total',
                     'Nb res.' = 'count',
                     'Moy.' = 'moy',
                     'Nb. FB' = 'nb_ep_fb',
                     'Part. FB' = 'part_fb',
                     'Nb. L' = 'nb_ep_l',
                     'Part. L' = 'part_l',
                     'Nb. M' = 'nb_ep_m',
                     'Part. M' = 'part_m',
                     'Nb. S' = 'nb_ep_s',
                     'Part. S' = 'part_s',
                     'Class. M' = 'class_m',
                     'Class. S' = 'class_s',
                     'Class. GT' = 'class_sgt',
                     'Cat. Championnat' = 'championnat')

  df %>%
    dplyr::rename(!!! rename_matrix[rename_matrix %in% colnames(.)]) %>%
    dplyr::rename_with(.fn = ~str_replace(.x, "lieu", "Conc. "),
                       .cols = starts_with("lieu")) %>%
    dplyr::rename_with(.fn = ~str_replace(.x, "per", "% "),
                       .cols = starts_with("per")) %>%
    dplyr::rename_with(.fn = ~str_replace(.x, "cat", "Ep. "),
                       .cols = starts_with("cat"))
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

#' Get FER U21 Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_u21_classes <- function() {
  return(pkg.env$ep_ch_u21)
}

#' Get FER L Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_l_classes <- function() {
  return(c("L14/60", "L15/40", "L16/60", "L18/60", "L19/60", "L20/60"))
}

#' Get FER M Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_m_classes <- function() {
  return(c("M24/60", "M25/60", "M26/60", "M27/60", "M28/60", "M29/60"))
}

#' Get FER S Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_s_classes <- function() {
  return(c("GEORG", "S1/60", "S10/60", "S31/60", "S32/60", "INT I"))
}

#' Get FER R Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_r_classes <- function() {
  return(pkg.env$ep_ch_r)
}

#' Get FER R Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_r_classes <- function() {
  return(pkg.env$ep_ch_r)
}

#' Get FER Swiss R Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_swiss_r_classes <- function() {
  return(pkg.env$ep_ch_m)
}

#' Get FER U21 Dressage classes
#'
#' @return A vector of strings
#' @export
get_fer_championship_n_classes <- function() {
  return(pkg.env$ep_ch_n)
}
