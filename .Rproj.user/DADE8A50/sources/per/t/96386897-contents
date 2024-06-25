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
        "Starter Composition."),
      bslib::card_body(

        fluidRow(
          column(4,
                 radioButtons(ns("starter_composition"), strong("Starter Composition Inputs:"),
                    choices = c("Manual" = "manual", "Lab results" = "lab"), inline = TRUE)),
          column(4,
                 radioButtons(ns("form_of_starter"), strong("Form of Starter:"),
                     choices = c("Pelleted" = "pelleted", "Texturized" = "texturized"), inline = TRUE)),
          column(4,
                 radioButtons(ns("cs_intake_equation"), strong("Starter Intake Equation:"),
                              choices = c("NASEM (2021)" = "NASEM", "Silva et al. (2019)" = "silva2019"), inline = TRUE))
        ),

        # Only show this panel if manual is selected
        conditionalPanel(
          condition = "input.starter_composition == 'manual'", ns = ns,
          bslib::card(
            fluidRow(
              column(2,
                     sliderInput(ns("CP"), label = h6(strong("CP (%):")), value = 21.2, min = 15, max = 30, step = 0.1)),
              column(2,
                     sliderInput(ns("NDF"), label = h6(strong("NDF (%):")), value = 12.9, min = 5, max = 30, step = 0.1)),
              column(2,
                     sliderInput(ns("NFC"), label = h6(strong("NFC (%):")), value = 55.79, min = 30, max = 70, step = 0.1)),
              column(2,
                     sliderInput(ns("EE"), label = h6(strong("Fat (%):")), value = 3.9, min = 2, max = 9, step = 0.1)),
              column(2,
                     sliderInput(ns("ash"), label = h6(strong("Ash (%):")), value = 6.21, min = 3, max = 10, step = 0.1)),
              column(2,
                     sliderInput(ns("starter_cost"), label = h6(strong("Cost ($/DM):")), value = 3.50, min = 1, max = 10, step = 0.1)),
              p("Remember: All components must sum up 100. The ash content will be calculated automatically.", style = "color: black; font-size: 14px;")
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

# -------------------------------------------------------------------------
# Adjusting the re-activity of the starter components ---------------------
# All components have to sum up 100 ---------------------------------------
# -------------------------------------------------------------------------

    observeEvent(c(input$CP, input$NDF, input$NFC, input$EE), {

      ash_cor <- 100 - input$CP - input$NDF - input$NFC - input$EE

      updateNumericInput(session, "ash", value = ash_cor, max = round(ash_cor + 3, 0), min = max(0, round(ash_cor - 3), 0))

    })

# -------------------------------------------------------------------------
# Reading the PDF file and extracting the chemical composition ------------
# -------------------------------------------------------------------------

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
          cs_cp = starter_composition_lab() |>
            dplyr::filter(Nutrients == "Crude Protein") |>
            dplyr::pull("Values (% DM)"),

          cs_ndf = starter_composition_lab() |>
            dplyr::filter(Nutrients == "Neutral Detergent Fiber") |>
            dplyr::pull("Values (% DM)"),

          cs_nfc = starter_composition_lab() |>
            dplyr::filter(Nutrients == "Non Fiber Carbohydrates") |>
            dplyr::pull("Values (% DM)"),

          cs_ee = starter_composition_lab() |>
            dplyr::filter(Nutrients == "Fat") |>
            dplyr::pull("Values (% DM)")
        )

      }

    })

    starter_composition_outputs <- reactive({
      list(
        cs_cp = starter_composition_list()$cs_cp,
        cs_ndf = starter_composition_list()$cs_ndf,
        cs_nfc = starter_composition_list()$cs_nfc,
        cs_ee = starter_composition_list()$cs_ee,
        cs_intake_equation = input$cs_intake_equation,
        form_of_starter = input$form_of_starter,
        starter_cost = input$starter_cost
      )
    })

    starter_composition_outputs

  })
}
