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

        fluidRow(
          column(6,
                 radioButtons(ns("starter_composition"), strong("Manual starter composition inputs or lab results?"),
                    choices = c("Manual" = "manual", "Lab results" = "lab"), inline = TRUE)),
          column(6,
                 radioButtons(ns("form_of_starter"), strong("Form of Starter:"),
                     choices = c("Pelleted" = "pelleted", "Texturized" = "texturized"), inline = TRUE))
        ),

        # Only show this panel if manual is selected
        conditionalPanel(
          condition = "input.starter_composition == 'manual'", ns = ns,
          bslib::card(
            fluidRow(
              column(3,
                     sliderInput(ns("CP"), label = h6(strong("Crude Protein (%):")), value = 21.2, min = 15, max = 30, step = 0.1)),
              column(3,
                     sliderInput(ns("NDF"), label = h6(strong("Neutral Detergent Fiber (%):")), value = 12.9, min = 5, max = 30, step = 0.1)),
              column(3,
                     sliderInput(ns("NFC"), label = h6(strong("Non Fiber Carbohydrates (%):")), value = 55.79, min = 30, max = 70, step = 0.1)),
              column(3,
                     sliderInput(ns("EE"), label = h6(strong("Fat (%):")), value = 3.9, min = 2, max = 9, step = 0.1))
            )
          )
        ),
        # Only show this panel if lab results is selected
        conditionalPanel(
          condition = "input.starter_composition == 'lab'", ns = ns,
          fluidRow(
            column(6,
                   DT::dataTableOutput(ns("starter_composition")),
                   br(),
                   fileInput(ns("file_input"), "Upload file (.pdf only)", accept = c(".pdf"))),
            p(strong("Functionality only implemented for Rock River Laboratory Inc. PDF reports."))
          )
        )
      )
    )
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

      pdf_text <- pdftools::pdf_text(input$file_input$datapath)

      pdf_text <- stringr::str_split(pdf_text, pattern = "\n")

      pdf_text

    })

    starter_composition_lab <- reactive({

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
            nutrient = c("Crude Protein", "Garbage", "Non Fiber Carbohydrates", "Neutral Detergent Fiber", "Fat"),
            values = ifelse(nutrient == "Crude Protein", column3,
                            ifelse(nutrient == "Fat", column3, column2)
            ),
            values = as.numeric(values)) |>
          dplyr::filter(nutrient != "Garbage") |>
          dplyr::select(nutrient, values) |>
          dplyr::rename(
            "Values (% DM)" = values,
            "Nutrients" = nutrient
          )
      )

    })

    output$starter_composition <- DT::renderDataTable({

      starter_composition_lab() |>
        DT::datatable(
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: left;",
            " ", htmltools::em(
              "Please double check whether the values displayed in the table match the values in the PDF report."
            )
          ),
          options = list(dom = "t"),
          rownames= FALSE
        )

    })

    starter_composition_list <- reactive({

      if(input$starter_composition == "manual"){

        list(
          cs_cp = input$CP,
          cs_ndf = input$NDF,
          cs_nfc = input$NFC,
          cs_ee = input$EE
        )

      } else {

        list(
          cs_cp = starter_composition_lab() |> dplyr::filter(Nutrients == "Crude Protein") |> dplyr::pull("Values (% DM)"),
          cs_ndf = starter_composition_lab() |> dplyr::filter(Nutrients == "Neutral Detergent Fiber") |> dplyr::pull("Values (% DM)"),
          cs_nfc = starter_composition_lab() |> dplyr::filter(Nutrients == "Non Fiber Carbohydrates") |> dplyr::pull("Values (% DM)"),
          cs_ee = starter_composition_lab() |> dplyr::filter(Nutrients == "Fat") |> dplyr::pull("Values (% DM)")
        )

      }

    })

    starter_composition_outputs <- reactive({
      list(
        cs_cp = starter_composition_list()$cs_cp,
        cs_ndf = starter_composition_list()$cs_ndf,
        cs_nfc = starter_composition_list()$cs_nfc,
        cs_ee = starter_composition_list()$cs_ee,
        form_of_starter = input$form_of_starter
      )
    })

    starter_composition_outputs

  })
}
