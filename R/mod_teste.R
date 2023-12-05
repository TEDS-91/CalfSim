#' teste UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teste_ui <- function(id){
  ns <- NS(id)
  tagList(

    numericInput(ns("input_teste"), label = "Input teste", value = 4),
    tableOutput(ns("tabela"))

  )
}

#' teste Server Functions
#'
#' @noRd
mod_teste_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dados_teste <- reactive({
      mtcars |>
        dplyr::filter(cyl < input$input_teste)
    })

    # output$tabela <- renderTable({
    #
    #   dados_teste()
    #
    # })

    # isso functiona
    dados_teste

    # Input <- reactive(input$input_teste)
    #
    # Input

    # outputs <- reactive({
    #   list(
    #     dataframe = dados_teste(),
    #     Input = Input()
    #   )
    # })
    #
    # outputs

    # return(
    #   list(
    #     #dados_teste = reactive( dados_teste ),
    #     meu_input = reactive( Input )
    #   )
    # )

  })
}

## To be copied in the UI
# mod_teste_ui("teste_1")

## To be copied in the server
# mod_teste_server("teste_1")
