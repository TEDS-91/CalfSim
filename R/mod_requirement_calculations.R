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

    #numericInput("liqDiet", "Milk or Milk Replacer (l/d):", 7),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Text"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1/5,
          numericInput(ns("BW"), label = h6("Birth Weight (kg):"), value = 40),
          numericInput(ns("liqDietME"), label = h6("Milk or Milk Replacer ME (Mcal/DM):"), value = 5),
          radioButtons(ns("liqDietOnly"), label = h6("Liquid Diet Only:"), choices = c("TRUE" = 1, "FALSE" = 2), selected = 1),
          numericInput(ns("nfc"), label = h6("NFC Starter (%):"), value = 60),
          numericInput(ns("liqDietDM"), label = h6("Milk or Milk Replacer DM:"), value = 0.12),
          numericInput(ns("temp"), label = h6("Aver. Temperature (°C):"), value = 15),
          numericInput(ns("weaning_age"), label = h6("Weaning Age (days):"), value = 56),
          numericInput(ns("age_simulated"), label = h6("Age simulated (days):"), value = 70),
          # Nutritional plans
          numericInput(ns("nut_plans"), label = h6("Number of nutritional plans:"), value = 3)
        )
      )
    ),
    uiOutput(ns("nutritional_plans_design")),
    tableOutput(ns("tabela"))

  )
}

#' inputs_page Server Functions
#'
#' @noRd
mod_inputs_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# -------------------------------------------------------------------------
# Defining the nutritional plan -------------------------------------------
# -------------------------------------------------------------------------

    output$nutritional_plans_design <- renderUI({

      nutritional_plans <- function() {

        tagList(

          number_of_nutritional_plans <- lapply(1:input$nut_plans,
                                                \(i) numericInput(ns(paste0("plan_", i)),
                                                                  label = paste0(i, "° phase (l/d):"),
                                                                  value = max((7 - i), 1))),
          days_of_change <- lapply(1:(input$nut_plans - 1),
                                   \(i) numericInput(ns(paste0("days_", i)),
                                                     label = paste0(i, "° change"),
                                                     value = ((i * 10) )))
        )

        return(list(
          number_of_nutritional_plans = number_of_nutritional_plans,
          days_of_change = days_of_change
        ))

      }

      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Definition of nutritional plans (whole milk or milk replacer)."
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header(
                class = "bg-dark",
                "Nutritional Plans"
              ),
              bslib::card_body(
                ifelse(input$nut_plans == 1,
                       list(numericInput(ns("onePlan"), label = "Milk Allowance (l/d):", 6)),
                       nutritional_plans()[1])
              )
            ),
            bslib::card(
              bslib::card_header(
                class = "bg-dark",
                "Days of change"
              ),
              bslib::card_body(
                ifelse(input$nut_plans == 1,
                       list(p("No change!")),
                       nutritional_plans()[2]),
                actionButton(ns("simulate"), label = "Simulate!", icon = tags$i(fontawesome::fa("person-running")))
              )
            )
          )
        )
      )

    })

    daily_milk_allowance <- reactive({

      req(input$days_1)

      nutritional_plans <- list()

      for(i in 1:input$nut_plans) {
        nutritional_plans[[i]] <- input[[paste0("plan_", i)]]
      }

      days_of_change <- list()

      if(input$nut_plans == 1) {

        days_of_change[[1]] <- 1

      } else {

        for(i in 1:(input$nut_plans - 1)) {

          days_of_change[[i]] <- input[[paste0("days_", i)]]

        }
      }

      if(input$nut_plans == 1) {

        milk_prg <- rep(input$onePlan, input$weaning_age)

      } else {

        milk_prg <- milk_allowance_program(weaning_age = input$weaning_age,
                                           nutritional_plans = input$nut_plans,
                                           milk_allowance = c(nutritional_plans |> unlist()),
                                           ages_of_change = c(days_of_change |> unlist()) )

      }

      milk_prg

    })

# -------------------------------------------------------------------------
# Calculating the nutritional requirements and nutrient supplied ----------
# -------------------------------------------------------------------------

    calculating_requirements <- eventReactive(input$simulate, {

      get_calf_requirements(liqDiet     = daily_milk_allowance(),
                            liqDietME   = input$liqDietME,
                            nfc_cs      = input$nfc,
                            liqDietDM   = input$liqDietDM,
                            initBW      = input$BW,
                            weaningAge  = input$weaning_age,
                            averTemp    = input$temp,
                            liqDietOnly = input$liqDietOnly,
                            max_size    = input$age_simulated)

    })

    # output$tabela <- renderTable({
    #
    #   calculating_requirements()
    #
    # })

    return(list(
      data_frame_simulated = reactive({ calculating_requirements() })
    ))

  })
}

