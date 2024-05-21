#' inputs_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inputs_page_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Animal, Management, Environmental, and Liquid Diet Inputs."),
      bslib::layout_column_wrap(
        width = NULL,
        height = 270,
        style = htmltools::css(grid_template_columns = "1.3fr 2fr"),
      bslib::card(
        fluidRow(
          column(4,
                 numericInput(ns("BW"), label = h6(strong("Birth Weight (kg):")), value = 40),
                 numericInput(ns("temp"), label = h6(strong("Aver. Temp. (C):")), value = 15)),
          column(4,
                 numericInput(ns("weaning_age"), label = h6(strong("Weaning Age (days):")), value = 65)),
          column(4,
              selectInput(ns("scenarios"), label = h6(strong("Number of Scenarios:")), choices = c(1, 2, 3, 4), selected = 1)))),
      bslib::card(
        mod_milk_composition_ui(ns("milk_milk_replacer_composition")))
      )
    ),
    mod_starter_composition_ui(ns("starter_composition")),
    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Scenarios for Milk Allowance Plans."),
      uiOutput(ns("nutritional_plans_design"))
    ),
    actionButton(ns("simulate"), label = "Simulate!", icon = tags$i(fontawesome::fa("person-running"))),
    textOutput(ns("data_preparation"))
  )
}

#' inputs_page Server Functions
#'
#' @noRd
mod_inputs_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# -------------------------------------------------------------------------
# Defining the milk composition -------------------------------------------
# -------------------------------------------------------------------------

    milk_composition <- mod_milk_composition_server("milk_milk_replacer_composition")

# -------------------------------------------------------------------------
# Defining the nutritional plan -------------------------------------------
# -------------------------------------------------------------------------

    all_scenarios <- reactive({

      all_scenarios <- lapply(1:input$scenarios, \(x) mod_dynamic_scenarios_server(paste0("scenario_", x)))

      })

    all_scenarios

    output$nutritional_plans_design <- renderUI({

      cenarios <- lapply(1:input$scenarios, \(x) mod_dynamic_scenarios_ui(ns(paste0("scenario_", x)), scenario_name = paste0("Scenario id ", x)))

        if(input$scenarios == 1) {

          tagList(
            bslib::layout_column_wrap(
              width = 1,
              height = 400,
            bslib::card(full_screen = TRUE,
                        bslib::card_header("Scenario 1"),
                        cenarios[[1]]))
          )
        } else if (input$scenarios == 2) {
          tagList(
            bslib::layout_column_wrap(
              width = 1/2,
              height = 400,
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 1"),
                          cenarios[[1]]),
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 2"),
                          cenarios[[2]])
            )
          )
        } else if(input$scenarios == 3) {
          tagList(
            bslib::layout_column_wrap(
              width = 1/3,
              height = 400,
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 1"),
                          cenarios[[1]]),
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 2"),
                          cenarios[[2]]),
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 3"),
                          cenarios[[3]])
            )
          )
        } else {
          tagList(
            bslib::layout_column_wrap(
              width = 1/4,
              height = 400,
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 1"),
                          cenarios[[1]]),
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 2"),
                           cenarios[[2]]),
              bslib::card(full_screen = TRUE,
                          bslib::card_header("Scenario 3"),
                          cenarios[[3]]),
            bslib::card(full_screen = TRUE,
                        bslib::card_header("Scenario 4"),
                        cenarios[[4]]))
          )
        }

    })

# -------------------------------------------------------------------------
# starter composition
# -------------------------------------------------------------------------

    starter_composition <- mod_starter_composition_server("starter_composition")

# -------------------------------------------------------------------------
# Milk allowance
# -------------------------------------------------------------------------

    milk_allowance_list <- reactive({

      milk_allowance_list <- lapply(1:input$scenarios,
                                    \(x) milk_allowance_program(weaning_age       = input$weaning_age,
                                                                nutritional_plans = all_scenarios()[[x]]()$nutritional_plans,
                                                                milk_allowance    = all_scenarios()[[x]]()$milk_allowance,
                                                                ages_of_change    = all_scenarios()[[x]]()$days_of_change ))



      milk_allowance_list

    })

    output$data_preparation <- renderText({

      all_scenarios()
      print(" ")

    })

# -------------------------------------------------------------------------
# Calculating the nutritional requirements and nutrient supplied ----------
# -------------------------------------------------------------------------

    calculating_requirements <- eventReactive(input$simulate, {

      dataframes_with_requirements <- lapply(1:input$scenarios,
                                             \(x) get_calf_requirements(liq_diet     = milk_allowance_list()[[x]],
                                                                        liq_diet_me  = milk_composition()[["milk_ME"]],
                                                                        starter_composition = list(
                                                                          cs_ndf = starter_composition()[["cs_ndf"]],
                                                                          cs_nfc = starter_composition()[["cs_nfc"]],
                                                                          cs_cp = starter_composition()[["cs_cp"]],
                                                                          cs_ee = starter_composition()[["cs_ee"]],
                                                                          form_of_starter = starter_composition()[["form_of_starter"]]
                                                                        ),
                                                                        cs_intake_equation = starter_composition()[["cs_intake_equation"]],
                                                                        liq_diet_dm = milk_composition()[["liq_diet_dm"]],
                                                                        initial_bw      = input$BW,
                                                                        weaning_age  = input$weaning_age,
                                                                        average_temperature    = input$temp,
                                                                        liq_diet_only = FALSE,
                                                                        max_size    = 100))


      # vector with the scenario names

      scenario_names <- lapply(1:input$scenarios, \(x) all_scenarios()[[x]]()$scenario_name)

      scenario_names <- unlist(scenario_names)

      # creating the dataframe with the requirements and scenarios

      milk_cost <- milk_composition()[["liq_diet_cost"]]

      starter_cost <- starter_composition()[["starter_cost"]]

      dataframes_with_requirements |>
        purrr::set_names(scenario_names) |>
        dplyr::bind_rows(.id = "scenario") |>
        dplyr::mutate(
          milk_cost = milk_cost,
          total_milk_cost = milk_cost * liq_diet_all,
          starter_cost = starter_cost,
          total_starter_cost = starter_cost * starter_intake
        )

    })

    output$requirements_table <- renderTable({

      calculating_requirements()

    })

# -------------------------------------------------------------------------
# Costs calculations ------------------------------------------------------
# -------------------------------------------------------------------------

    costs <- reactive({

      milk_cost <- milk_composition()[["liq_diet_cost"]]

      calculating_requirements()

    })


    return(
      list(
      data_frame_simulated = reactive({ calculating_requirements() })
    )
  )

  })
}

