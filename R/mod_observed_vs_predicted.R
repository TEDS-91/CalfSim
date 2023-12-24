#' observed_vs_predicted UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_observed_vs_predicted_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Observed vs Predicted."),
      height = 800,
      bslib::card_body(

        fileInput(ns("observedFile"), "Choose xlsx file",
                  accept = c(".xlsx")),

        downloadButton(ns("templateFile"),
                       "Download Template!")
      )
    )

  )
}

#' observed_vs_predicted Server Functions
#'
#' @noRd
mod_observed_vs_predicted_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    templateFile <- readxl::read_xlsx("data-raw/dataEntryTemplateCalfSim.xlsx")

    output$templateFile <- downloadHandler(

      filename = function() {

        paste("dataEntryTemplateCalfSim", ".xlsx")

      },
      content = function(file) {

        writexl::write_xlsx(templateFile, path = file)

      }
    )

  })
}
