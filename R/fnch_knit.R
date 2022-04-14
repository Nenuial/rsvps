#' Return full path to geovizr ressources
#'
#' @return A string with the full path
#' @keywords internal
tex_global_path <- function() {
  paste0(system.file("rmarkdown/resources", package = "rsvps"), "/")
}

#' Custom Knit function for RStudio
#'
#' @export
knit_quiet <- function(input, ...) {
  rmarkdown::render(
    input,
    quiet = TRUE,
    clean = TRUE,
    envir = globalenv()
  )

  yaml <- rmarkdown::yaml_front_matter(input)

  if (!is.null(yaml$subfile)) {
    yaml$subfile %>%
      purrr::iwalk(
        .f = ~ knit_subfile(input = input, output = yaml[["output"]], key = .y, value = .x)
      )
  }

  knit_cleanup(input)
}

#' Knit subfiles
#'
#' @param input The input file
#' @param keyword The subfile head as a named vector
#' @keywords internal
knit_subfile <- function(input, output, key, value) {
  output_args <- list(
    metadata = rmarkdown::pandoc_metadata_arg(
      name = "subclass",
      value = value
    )
  )

  rmarkdown::render(
    input = input,
    output_options = output_args,
    output_file = paste0(
      fs::path_ext_remove(input),
      "_", key
    ),
    quiet = TRUE,
    clean = TRUE,
    envir = globalenv()
  )
}

#' Remove all temporary files
#'
#' @param path A folder to check for files
#' @keywords internal
knit_cleanup <- function(input) {
  fs::path_dir(path = input) %>%
    fs::dir_ls(regexp = "[.](log|ent|tns|mst)") %>%
    fs::file_delete()
}

#' Knit all Rmd files in the Project
#'
#' @param path The path to start at
#'
#' @export
knit_all <- function(path = here::here()) {
  fs::dir_ls(path = path, recurse = TRUE, regexp = ".*\\.Rmd") %>%
    purrr::walk(~knit_quiet(.x, encoding = "UTF-8"))
}
