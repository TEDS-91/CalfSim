#' descriptive_pre_weaning_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descriptive_pre_weaning_performance_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Table with all data simulated."),
      height = 800,
      bslib::card_body(
        min_height = 500,
        DT::DTOutput(ns("table")),
      )
    ),

  hr(),

  fluidRow(column(offset = 10, 2, downloadButton(ns("downloadData"), label = "Download!")))

  )
}

#' descriptive_pre_weaning_performance Server Functions
#'
#' @noRd
mod_descriptive_pre_weaning_performance_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- DT::renderDT({

      dataset() |>

        dplyr::group_by(scenario) |>
        dplyr::filter(daysOfLife <=56) |>
        dplyr::summarise(
          "Initial Body Weight (kg)" = dplyr::first(BW),
          "Final Body Weight (kg)" = dplyr::last(BWcor),
          "Av. Daily Gain (kg)" = (dplyr::last(BWcor) - dplyr::first(BW)) / max(daysOfLife),
          "Av. Daily Feed Intake (kg)" = sum(totalDMI) / max(daysOfLife),
          "Feed Efic. (kg/kg)" = sum(totalDMI) / (dplyr::last(BWcor) - dplyr::first(BW)),
          "Age at Weaning (days)" = max(daysOfLife),
          "Total Milk Consumption (kg)" = sum(LiqDietAll)
        ) |>
        tidyr::pivot_longer(
          cols = -scenario,
          names_to = "Variable",
          values_to = "Value"
        ) |>
        tidyr::pivot_wider(
          names_from = scenario,
          values_from = Value
        ) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

    }, options = list(paging = TRUE,
                      scrollY = "600px",
                      scrollX = "900px",
                      pageLength = 20,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#007582', 'color': '#fff'});",
                        "}")),
    style = "bootstrap5")

  })
}

## To be copied in the UI
# mod_descriptive_pre_weaning_performance_ui("descriptive_pre_weaning_performance_1")

## To be copied in the server
# mod_descriptive_pre_weaning_performance_server("descriptive_pre_weaning_performance_1")
