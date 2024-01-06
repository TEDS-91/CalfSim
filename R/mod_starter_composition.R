#' starter_composition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_starter_composition_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Starter chemical composition."),
      bslib::card_body(
        bslib::card(
          fluidRow(
          column(3,
                 sliderInput(ns("CP"), label = h6("Crude Protein (%):"), value = 21.2, min = 15, max = 30, step = 0.1)),
          column(3,
                 sliderInput(ns("NDF"), label = h6("Neutral Detergent Fiber (%):"), value = 12.9, min = 5, max = 30, step = 0.1)),
          column(3,
                sliderInput(ns("NFC"), label = h6("Non Fiber Carbohydrates (%):"), value = 55.79, min = 30, max = 70, step = 0.1)),
          column(3,
                 sliderInput(ns("EE"), label = h6("Fat (%):"), value = 3.9, min = 2, max = 9, step = 0.1)),
          column(3,
                 fileInput(ns("file_input"), "upload file ( . pdf format only)", accept = c(".pdf")))
          )
        )
      )
    ),

    tableOutput(ns("starter_composition"))

  )
}

#' starter_composition Server Functions
#'
#' @noRd
mod_starter_composition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    input_pdf_file <- reactive({
      req(input$file_input)

      txt <- pdftools::pdf_text(input$file_input$datapath)

      txt <- stringr::str_split(txt, pattern = "\n")

      txt

    })

    starter_composition_lab <- reactive({

      filtering_strings <- function(vector, text_pattern) {
        vector |>
          unlist() |>
          tibble::as_tibble() |>
          dplyr::filter(str_detect(value, text_pattern))
      }


      list_of_components <- list(
        "Crude Protein",
        "NFC",
        "aNDF",
        "Fat"
      ) |>
        lapply(filtering_strings, vector = input_pdf_file()) |>
        dplyr::bind_rows()

      suppressWarnings(
        list_of_components |>
          tidyr::separate(value, into = c("text", "column2", "column3", "column4"), sep = "\\s+") |>
          dplyr::mutate(
            nutrient = c("CP", "Garbage", "NFC", "NDF", "Fat"),
            values = ifelse(nutrient == "CP", column3,
                            ifelse(nutrient == "Fat", column3, column2)
            ),
            values = as.numeric(values)) |>
          dplyr::filter(nutrient != "Garbage") |>
          dplyr::select(nutrient, values)
      )

    })

    output$starter_composition <- renderTable({

      starter_composition_lab()

    })

    # output$starter_composition <- renderPrint({
    #   paste0("CP: ", input$CP, " NDF: ", input$NDF, " NFC: ", input$NFC, " EE: ", input$EE)
    # })


    starter_composition_outputs <- reactive({
      list(
        cs_cp = input$CP,
        cs_ndf = input$NDF,
        cs_nfc = input$NFC,
        cs_ee = input$EE
      )
    })

    starter_composition_outputs

  })
}

## To be copied in the UI
# mod_starter_composition_ui("starter_composition_1")

## To be copied in the server
# mod_starter_composition_server("starter_composition_1")
