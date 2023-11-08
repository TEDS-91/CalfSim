#' data_table UI Function
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


    bslib::accordion(
      bslib::accordion_panel(
        "Simulated",
        bslib::card(
          bslib::card_header(
            class = "bg-dark",
            "Table with all data simulated."),
          height = 800,
          bslib::card_body(
            min_height = 500,
            DT::DTOutput(ns("table")),
          )
        )
      )
    ),
    hr(),
    fluidRow(column(offset = 10, 2, downloadButton(ns("downloadData"), label = "Download!")))

  )
}

#' data_table Server Functions
#'
#' @noRd
mod_data_table_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- DT::renderDT({

      dataset() |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

    }, options = list(paging = TRUE,
                      scrollY = "600px",
                      scrollX = "900px",
                      pageLength = 20),
    style = "bootstrap5")

    output$downloadData <- downloadHandler(

      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("DataSimulated", ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        utils::write.csv(dataset(), file)
      }
    )

  })
}
