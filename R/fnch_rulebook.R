#' Standard pdf document
#'
#' @param metadata Additional pandoc metadata
#' @export
fnch_rulebook <- function(..., metadata = c()) {
  template <- fnch_file("rmarkdown/templates/Rulebook/resources/rulebook_template.tex")

  fnch_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Render pdf documents
#'
#' @param template_path Path of the latex template
#' @param metadata Vector with pandoc metadata
#' @keywords internal
fnch_render_pdf_document <- function(..., template_path, metadata) {
  project_metadata <- knit_metadata(fs::path_dir(template_path))
  project_metadata <- c(project_metadata, metadata)

  bookdown::pdf_document2(
    ...,
    template = template_path,
    latex_engine = "xelatex",
    citation_package = "biblatex",
    pandoc_args = project_metadata
  )
}


#' Generate pandoc metadata
#'
#' @param path Path of template files
#'
#' @return A vector with pandoc metadata arguments
#' @keywords internal
knit_metadata <- function(path) {
  metadata <- c()

  if(fs::file_exists(here::here("_document.yaml"))) {
    yaml::read_yaml(here::here("_document.yaml")) %>%
      purrr::imap(~rmarkdown::pandoc_metadata_arg(
        name = .y,
        value = .x
      )) %>% purrr::as_vector() %>% unname() -> metadata
  }

  c(metadata,
    rmarkdown::pandoc_metadata_arg(name = "globalpath", value = tex_global_path()),
    rmarkdown::pandoc_metadata_arg(name = "templatepath",
                                   value = paste0(path, "/")),
    rmarkdown::pandoc_metadata_arg(name = "csquotes"),
    rmarkdown::pandoc_lua_filter_args(fnch_file("rmarkdown/lua/rulebooks.lua"))) -> metadata

  return(metadata)
}
