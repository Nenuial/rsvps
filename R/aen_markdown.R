#' AEN pdf letter
#'
#' @param metadata Additional pandoc metadata
#' @export
aen_letter <- function(..., metadata = c()) {
  metadata <- c(
    metadata,
    rmarkdown::pandoc_metadata_arg(name = "lang",
                                   value = "french"),

    rmarkdown::pandoc_metadata_arg(name = "logo",
                                   value = fnch_file("rmarkdown/resources/images/aen.pdf")),
    rmarkdown::pandoc_metadata_arg(name = "logo-size",
                                   value = "width=3cm"),

    rmarkdown::pandoc_metadata_arg(name = "author",
                                   value = "Pascal Burkhard"),
    rmarkdown::pandoc_metadata_arg(name = "mytitle",
                                   value = "Président"),
    rmarkdown::pandoc_metadata_arg(name = "return-email",
                                   value = "president@aen-ne.ch"),
    rmarkdown::pandoc_metadata_arg(name = "return-phone",
                                   value = "079 772 37 56"),
    rmarkdown::pandoc_metadata_arg(name = "return-url",
                                   value = "www.aen-ne.ch"),
    rmarkdown::pandoc_metadata_arg(name = "return-address",
                                   value = "Ch. du Marais 10"),
    rmarkdown::pandoc_metadata_arg(name = "return-address",
                                   value = "1031 Mex")
  )

  fnch_letter_standard(..., metadata = metadata)
}
