#' Dynamic Scenarios UI Function
#'
#' @description This module builds the structure of aech scenario, returning all inputs to populate other functions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dynamic_scenarios_ui <- function(id, scenario_name = "Scenario id"){
  ns <- NS(id)
  tagList(
    bslib::card(
      fluidRow(
        column(6,
               numericInput(ns("number_of_nut_plans"), label = h6("Nut. Plans:"), value = 2)),
        column(6,
               textInput(ns("scenario_name"), label = h6("Scenario:"), value = scenario_name))
      ),
      uiOutput(ns("nutritional_plans_design")),
    )
  )
}

#' Dynamic Scenario Server Functions
#'
#' @noRd
mod_dynamic_scenarios_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$nutritional_plans_design <- renderUI({

        tagList(

          # Adjust to the case of only one scenario

          fluidRow(


            column(6,
                   lapply(1:input$number_of_nut_plans, \(i) numericInput(ns(paste0("plan_", i)),
                                                                         label = paste0(i, "° phase (l/d):"),
                                                                         value = max((7 - i), 1)))),
            column(6,
                   lapply(1:(input$number_of_nut_plans - 1), \(i) numericInput(ns(paste0("days_", i)),
                                                                               label = paste0(i, "° change"),
                                                                               value = ((i * 10) ))))
          )
        )

    })

    milk_allowance <- reactive({

      req(input$number_of_nut_plans)

      milk_allowance <- lapply(1:input$number_of_nut_plans, \(i) input[[paste0("plan_", i)]])

      milk_allowance <- unlist(milk_allowance)

      milk_allowance

    })

    ages_of_change <- reactive({

      req(input$number_of_nut_plans)

      ages_of_change <- lapply(1:(input$number_of_nut_plans - 1), \(i) input[[paste0("days_", i)]])

      ages_of_change <- unlist(ages_of_change)

      ages_of_change

    })

    scenario_output <- reactive({
      list(
        scenario_name     = input$scenario_name,
        nutritional_plans = input$number_of_nut_plans,
        milk_allowance    = milk_allowance(),
        days_of_change    = ages_of_change()
      )
    })

    scenario_output

  })
}

