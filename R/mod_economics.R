#' economics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_economics_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-green",
        "Economics."),
      bslib::card_body(
        numericInput(ns("milk_cost"), label = "Milk Price or MR ($/l):", value = 0.35, min = 0, max = 1),
        numericInput(ns("calf_starter_cost"), label = "Starter Cost ($/kg):", value = 100, min = 0, max = 1000),
      )
    )

  )
}

#' economics Server Functions
#'
#' @noRd
mod_economics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
