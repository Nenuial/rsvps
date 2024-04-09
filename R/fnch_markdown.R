#' FNCH rulebook pdf
#'
#' @param metadata Additional pandoc metadata
#' @export
fnch_rulebook <- function(..., metadata = c()) {
  template <- fnch_file("rmarkdown/templates/Rulebook/resources/rulebook_template.tex")

  metadata <- c(metadata, rmarkdown::pandoc_lua_filter_args(fnch_file("rmarkdown/lua/rulebooks.lua")))

  fnch_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' Standard pdf document
#'
#' @param metadata Additional pandoc metadata
#' @export
fnch_document <- function(..., metadata = c()) {
  template <- fnch_file("rmarkdown/templates/Document/resources/document_template.tex")

  fnch_render_pdf_document(..., template_path = template, metadata = metadata)
}

#' FNCH pdf letter
#'
#' @param metadata Additional pandoc metadata
#' @export
fnch_letter <- function(..., metadata = c()) {
  metadata <- c(
    metadata,
    rmarkdown::pandoc_metadata_arg(name = "lang",
                                   value = "french"),

    rmarkdown::pandoc_metadata_arg(name = "logo",
                                   value = fnch_file("rmarkdown/resources/images/fsse.pdf")),
    rmarkdown::pandoc_metadata_arg(name = "logo-size",
                                   value = "width=2.5cm"),

    rmarkdown::pandoc_metadata_arg(name = "author",
                                   value = "Pascal Burkhard"),
    rmarkdown::pandoc_metadata_arg(name = "return-email",
                                   value = "pascal.burkhard@gmail.com"),
    rmarkdown::pandoc_metadata_arg(name = "return-phone",
                                   value = "079 772 37 56"),
    rmarkdown::pandoc_metadata_arg(name = "return-url",
                                   value = "www.swiss-equestrian.ch"),
    rmarkdown::pandoc_metadata_arg(name = "return-address",
                                   value = "Ch. du Marais 10"),
    rmarkdown::pandoc_metadata_arg(name = "return-address",
                                   value = "1031 Mex")
  )

  fnch_letter_standard(..., metadata = metadata)
}

#' Standard pdf letter
#'
#' @param metadata Additional pandoc metadata
#' @export
fnch_letter_standard <- function(..., metadata = c()) {
  template <- fnch_file("rmarkdown/templates/Letter/resources/letter_template.tex")

  lco_default <- fnch_file("rmarkdown/templates/Letter/resources/swiss.lco")
  lco_default <- sub("\\.[^.]*$", "", lco_default)

  metadata <- c(
    metadata,
    rmarkdown::pandoc_metadata_arg(name = "csquotes"),
    rmarkdown::pandoc_metadata_arg(name = "lco_default",
                                   value = lco_default),
    rmarkdown::pandoc_metadata_arg(name = "papersize",
                                   value = "a4")
  )

  base <- inherit_pdf_document(..., template=template,
                               latex_engine = "xelatex",
                               md_extensions=c("-autolink_bare_uris"),
                               pandoc_args = metadata)

  return(base)
}

#' Rmarkdown pdf document
#'
#' Call rmarkdown::pdf_document and mark the return
#' value as inheriting pdf_document
#'
#' @param ...
#'
#' @keywords internal
inherit_pdf_document <- function(...){
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"

  return(fmt)
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
    rmarkdown::pandoc_metadata_arg(name = "csquotes")) -> metadata

  return(metadata)
}

#' FSSE html jury plan
#'
#' @export
fnch_html_jury <- function(...) {
  rmarkdown::html_document(
    ...,
    css = fnch_file("rmarkdown/templates/Jury/resources/style.css")
  )
}
