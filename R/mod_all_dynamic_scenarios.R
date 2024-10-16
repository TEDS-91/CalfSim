#' all_dynamic_scenarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_all_dynamic_scenarios_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             numericInput(ns("initial_bw"), "Initial Body Weight (kg)", value = 40, min = 20, max = 100, step = 1)),
      column(3,
             numericInput(ns("weaning_age"), "Weaning Age (days)", value = 60, min = 30, max = 120, step = 1)),
      column(3,
             numericInput(ns("aver_temp"), "Average Temperature (°C)", value = 20, min = 0, max = 40, step = 1)),
      column(3,
             selectInput(ns("n_scenarios"), "Number of scenarios", selected = 1, choices = 1:3))
    ),
    uiOutput(ns("dynamic_scenarios")),
    uiOutput(ns("nutritional_plans_design")),
    tableOutput(ns("show_inputs"))

  )
}

#' all_dynamic_scenarios Server Functions
#'
#' @noRd
mod_all_dynamic_scenarios_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # show all inputs

    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
      data.frame(
        names = names(x),
        values = unlist(x, use.names = FALSE)
      )
    })

    output$show_inputs <- renderTable({
      all_scen <- all_scen()
      AllInputs()

    })


    output$dynamic_scenarios <- renderUI({

      tagList(
        lapply(1:input$n_scenarios, function(i){
            mod_all_inputs_scenarios_ui(paste0(ns("milk_composition"), i))
        })
      )

    })

    all_scen <- reactive({
      lapply(1:input$n_scenarios, function(i){
        mod_all_inputs_scenarios_server(paste0("milk_composition", i))})
    })



  })
}

## To be copied in the UI
# mod_all_dynamic_scenarios_ui("all_dynamic_scenarios_1")

## To be copied in the server
# mod_all_dynamic_scenarios_server("all_dynamic_scenarios_1")
