#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  myFunction <- function(nome, func_input) {
    output[[ nome ]] <- renderText(input[[func_input]])
  }

  list(
    nome = c("teste1", "teste2", "teste3"),
    func_input = c("tt1", "tt2", "tt3")
  ) |>
    purrr::pmap(myFunction)

}
