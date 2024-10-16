#' all_inputs_scenarios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_all_inputs_scenarios_ui <- function(id){
  ns <- NS(id)
  tagList(

    selectInput(ns("liq_diet"), strong("Whole Milk or Milk Replacer:"),
                choices = c("Whole Milk" = "whole", "Milk Replacer" = "milk_replacer")
    ),

    # Only show this panel if whole milk is selected
    conditionalPanel(
      condition = "input.liq_diet == 'whole'", ns = ns,
      fluidRow(
        column(2,
               numericInput(ns("milk_protein"), label = h6(strong("Protein (%):")), value = 3.2)),
        column(2,
               numericInput(ns("milk_fat"), label = h6(strong("Fat (%):")), value = 3.8)),
        column(2,
               numericInput(ns("milk_ash"), label = h6(strong("Ashes (%):")), value = 0.78)),
        column(3,
               numericInput(ns("total_solids"), label = h6(strong("Total Solids (%)")), value = 12.5)),
        column(3,
               numericInput(ns("milk_price"), label = h6(strong("Price ($/cwt):")), value = 22))
      )),

    # Only show this panel if milk replacer is selected
    conditionalPanel(
      condition = "input.liq_diet == 'milk_replacer'", ns = ns,
      fluidRow(
        column(2,
               numericInput(ns("milk_rep_protein"), label = h6(strong("Protein (%):")), value = 21.7)),
        column(2,
               numericInput(ns("milk_rep_fat"), label = h6(strong("Fat (%):")), value = 18.6)),
        column(2,
               numericInput(ns("milk_rep_ash"), label = h6(strong("Ashes (%):")), value = 7.6)),
        column(2,
               numericInput(ns("dry_matter"), label = h6(strong("DM (%)")), value = 96)),
        column(2,
               numericInput(ns("liqDietDilution"), label = h6(strong("Rep. Dilution:")), value = 0.125)),
        column(2,
               numericInput(ns("milk_rep_price"), label = h6(strong("Price ($/kg DM):")), value = 4.50))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-green",
        "Starter Composition."),
      bslib::card_body(

        fluidRow(
          column(4,
                 radioButtons(ns("starter_composition"), strong("Starter Composition Inputs:"),
                              choices = c("Manual" = "manual", "Lab results" = "lab"), inline = TRUE)),
          column(4,
                 radioButtons(ns("form_of_starter"), strong("Form of Starter:"),
                              choices = c("Pelleted" = "pelleted", "Texturized" = "texturized"), inline = TRUE)),
          column(4,
                 radioButtons(ns("cs_intake_equation"), strong("Starter Intake Equation:"),
                              choices = c("NASEM (2021)" = "NASEM", "Silva et al. (2019)" = "silva2019"), inline = TRUE))
        ),

        # Only show this panel if manual is selected
        conditionalPanel(
          condition = "input.starter_composition == 'manual'", ns = ns,
          bslib::card(
            fluidRow(
              column(2,
                     numericInput(ns("CP"), label = h6(strong("CP (%):")), value = 21.2, min = 15, max = 30, step = 0.1)),
              column(2,
                     numericInput(ns("NDF"), label = h6(strong("NDF (%):")), value = 12.9, min = 5, max = 30, step = 0.1)),
              column(2,
                     numericInput(ns("NFC"), label = h6(strong("NFC (%):")), value = 55.79, min = 30, max = 70, step = 0.1)),
              column(2,
                     numericInput(ns("EE"), label = h6(strong("Fat (%):")), value = 3.9, min = 2, max = 9, step = 0.1)),
              column(2,
                     numericInput(ns("ash"), label = h6(strong("Ash (%):")), value = 6.21, min = 3, max = 10, step = 0.1)),
              column(2,
                     numericInput(ns("starter_cost"), label = h6(strong("Cost ($/DM):")), value = 3.50, min = 1, max = 10, step = 0.1)),
              p("Remember: All components must sum up 100. The ash content will be calculated automatically.", style = "color: black; font-size: 14px;")
            )
          )
        ),
        # Only show this panel if lab results is selected
        # conditionalPanel(
        #   condition = "input.starter_composition == 'lab'", ns = ns,
        #   fluidRow(
        #     column(6,
        #            DT::dataTableOutput(ns("starter_composition")),
        #            br(),
        #            fileInput(ns("file_input"), "Upload file (.pdf only)", accept = c(".pdf"))),
        #     p(strong("Functionality only implemented for Rock River Laboratory Inc. PDF reports."))
        #   )
        # )
        fluidRow(
          column(6,
                 numericInput(ns("number_of_nut_plans"), label = h6(strong("Liquid Diet Feeding Plan:")), value = 2)),
          column(6,
                 textInput(ns("scenario_name"), label = h6(strong("Scenario:")), value = "scenario_name"))
        ),
        fluidRow(
          uiOutput(ns("nutritional_plans_design"))
        )
      )
    )

  )
}

#' all_inputs_scenarios Server Functions
#'
#' @noRd
mod_all_inputs_scenarios_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$nutritional_plans_design <- renderUI({

        # Adjust to the case of only one scenario

        tagList(
          column(6,
                 lapply(1:input$number_of_nut_plans, \(i) numericInput(ns(paste0("plan_", i)),
                                                                       label = strong(paste0(i, "° Phase (l/d):")),
                                                                       value = max((7 - i), 1)))),

          column(6,
                 lapply(1:(input$number_of_nut_plans - 1), \(i) numericInput(ns(paste0("days_", i)),
                                                                             label = strong(paste0(i, "° Change (days):")),
                                                                             value = ((i * 10) ))))
        )

    })

    milk_allowance <- reactive({

     req(input$number_of_nut_plans)

     milk_allowance <- lapply(1:input$number_of_nut_plans, \(i) input[[paste0("plan_", i)]])

     milk_allowance <- unlist(milk_allowance)

     print(milk_allowance)

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

## To be copied in the UI
# mod_all_inputs_scenarios_ui("all_inputs_scenarios_1")

## To be copied in the server
# mod_all_inputs_scenarios_server("all_inputs_scenarios_1")
