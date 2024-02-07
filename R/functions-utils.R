
#' Filtering the strings in a pdf file - functiong used in the starter composition module.
#'
#' @param vector Vector containing the pdf file content.
#' @param text_pattern Text pattern to be searched in the pdf file.
#'
#' @return The filtered strings.

filtering_strings <- function(vector, text_pattern) {
  vector |>
    unlist() |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_detect(value, text_pattern))
}

