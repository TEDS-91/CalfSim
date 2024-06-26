#' milk_composition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_milk_composition_ui <- function(id){
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
    tableOutput(ns("tabela"))

  )
}

#' milk_composition Server Functions
#'
#' @noRd
mod_milk_composition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    metabolizable_energy <- reactive({

      if(input$liq_diet == "whole") {
        metabolizable_energy <- milk_metabolizable_energy(protein = input$milk_protein,
                                                          fat = input$milk_fat,
                                                          ashes = input$milk_ash,
                                                          total_solids = input$total_solids)
      } else {
        metabolizable_energy <- milk_replacer_metabolizable_energy(protein = input$milk_rep_protein,
                                                                   fat = input$milk_rep_fat,
                                                                   ashes = input$milk_rep_ash,
                                                                   total_solids = input$dry_matter)
      }

      print(metabolizable_energy)

    })

    liq_diet_dm_cost <- reactive({

      if(input$liq_diet == "whole") {

        liq_diet_dm <- input$total_solids / 100

        liq_diet_cost <- input$milk_price / 45.36

        list(liq_diet_dm = liq_diet_dm, liq_diet_cost = liq_diet_cost)

      } else {

        liq_diet_dm <- input$liqDietDilution

        liq_diet_cost <- input$milk_rep_price * input$liqDietDilution

        list(liq_diet_dm = liq_diet_dm, liq_diet_cost = liq_diet_cost)

      }

    })

    milk_composition_outputs <- reactive({
      list(
        milk_ME = metabolizable_energy(),
        liq_diet_dm = liq_diet_dm_cost()$liq_diet_dm,
        liq_diet_cost = liq_diet_dm_cost()$liq_diet_cost
      )
    })

    milk_composition_outputs

  })
}
