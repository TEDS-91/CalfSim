#' Data Table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "bg-green",
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
mod_data_table_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- DT::renderDT({

      dataset() |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

    }, options = list(dom = 't',
                      ordering = FALSE,
                      paging = TRUE,
                      scrollY = "600px",
                      scrollX = "900px",
                      pageLength = 20,
    initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#007582', 'color': '#fff'});",
      "}")),
    style = "bootstrap5")

    # Download data

    output$downloadData <- downloadHandler(

      filename = function() { "dataSimulated.xlsx"},

      content = function(file) {
        writexl::write_xlsx(dataset(), path = file)
        }
    )


  })
}
