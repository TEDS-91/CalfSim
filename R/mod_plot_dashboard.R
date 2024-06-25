#' dashboard plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_plot_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-green",
        "Select the Variables to be Plotted."
      ),
      bslib::card_body(
        min_height = 100,
        max_height = 450,
        uiOutput(ns("var_selection")),
        bslib::layout_column_wrap(
          width = 1/2,
          echarts4r::echarts4rOutput(ns("echarts_plot1")),
          echarts4r::echarts4rOutput(ns("echarts_plot2")),
        )
      )
    )
  )
}

#' dashboard plot Server Functions
#'
#' @noRd
mod_dashboard_plot_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    var_renamed_dataset <- reactive({

      dataset() |>
        dplyr::rename(
          "Scenario" = scenario,
          "Days on Feed" = days_of_life,
          "Body Weight (kg)" = BW,
          "Average Daily Gain (kg)" = ADG,
          "Met. Ener. Req. Maint. (Mcal)" = me_maintenance,
          "Met. Ener. Req. Gain (Mcal)" = me_balance,
          "Liq. Diet Intake (kgDM)" = liq_diet_intake,
          "Met. Ener. from Liq. Diet Intake (Mcal)" = me_from_liq_diet,
          "Starter Intake (kg)" = starter_intake,
          "Met. Ener. from Starter Intake (Mcal)" = me_from_starter_intake,
          "Total Dry Matter Intake (kg)" = total_dmi,
          "NFC Intake cum. (kg)" = nfc_intake_cum,
          "Total Met. Ener. Intake (Mcal)" = total_me_intake,
          "Met. Protein Req. Maint. (kg)" = met_protein_maintenance,
          "Met. Protein Req. Gain (kg)" = met_protein_gain,
          "Total Met. Protein Req. (kg)" = total_met_protein_req,
          "Fat Gain (kg)" = fat_gain,
          "Protein Gain (kg)" = protein_gain
        ) |>
        dplyr::select(
          "Scenario",
          "Days on Feed",
          "Body Weight (kg)",
          "Average Daily Gain (kg)",
          "Met. Ener. Req. Maint. (Mcal)",
          "Met. Ener. Req. Gain (Mcal)",
          "Liq. Diet Intake (kgDM)",
          "Met. Ener. from Liq. Diet Intake (Mcal)",
          "Starter Intake (kg)",
          "Met. Ener. from Starter Intake (Mcal)",
          "Total Dry Matter Intake (kg)",
          "NFC Intake cum. (kg)",
          "Total Met. Ener. Intake (Mcal)",
          "Met. Protein Req. Maint. (kg)",
          "Met. Protein Req. Gain (kg)",
          "Total Met. Protein Req. (kg)",
          "Fat Gain (kg)",
          "Protein Gain (kg)"
        )
    })


    output$var_selection <- renderUI({

      tagList(
        fluidRow(
          column(4,
                 selectInput(ns("var_2"), "Select 1° Y axis Variable (left):", names(var_renamed_dataset() |>
                                                                                     dplyr::select(-Scenario, -`Days on Feed`)),
                             selected = names(var_renamed_dataset()["Body Weight (kg)"]))),
          column(4,
                 selectInput(ns("var_3"), "Select 2° Y axis Variable (right):", names(var_renamed_dataset() |>
                                                                                      dplyr::select(-Scenario, -`Days on Feed`)),
                             selected = names(var_renamed_dataset()["Average Daily Gain (kg)"])))
        )
      )
    })

    output$echarts_plot1 <- echarts4r::renderEcharts4r({

      req(input$var_2)

      plot_left <- scenario_plot(var_renamed_dataset(), input$var_2)

      plot_left

    })

    output$echarts_plot2 <- echarts4r::renderEcharts4r({

      req(input$var_3)

      plot_right <- scenario_plot(var_renamed_dataset(), input$var_3)

      plot_right

    })

  })
}
