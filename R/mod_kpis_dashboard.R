#' kpis_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_kpis_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_column_wrap(

      width = "250px",
      bslib::value_box(
        title = "Final Body weight (kg)",
        value = textOutput(ns("fBW")),
        showcase = bsicons::bs_icon("graph-up")
      ),
      bslib::value_box(
        title = "Average Daily Gain (kg)",
        value = textOutput(ns("ADG")),
        showcase = bsicons::bs_icon("graph-up")
      ),
      bslib::value_box(
        title = "Average Daily Starter Intake (kg)",
        value = textOutput(ns("ST")),
        showcase = bsicons::bs_icon("bucket")
      ),
      bslib::value_box(
        title = "Days to NFC 15 kg",
        value = textOutput(ns("nfcCum")),
        showcase = bsicons::bs_icon("bucket")
      ),
      bslib::value_box(
        title = "Whole Milk/Milk Replacer ME (Mcal/kg)",
        value = textOutput(ns("milkME")),
        showcase = bsicons::bs_icon("fuel-pump")
      )
    ),

    # fluidRow(
    #   mod_teste_ui(ns("teste_1"))
    # ),

    tableOutput(ns("show_inputs")),
    tableOutput(ns("teste"))
  )
}

#' kpis_dashboard Server Functions
#'
#' @noRd
mod_kpis_dashboard_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
      data.frame(
        names = names(x),
        values = unlist(x, use.names = FALSE)
      )
    })

    # dados <- mod_teste_server("teste_1")
    #
    # output$show_inputs <- renderTable({
    #   dados()[[1]]
    # })
    #
    # dados

    # teste2 <- reactive({
    #
    #   #dados() |> tibble::as_tibble()
    #
    # })

    kpis_calculations <- reactive({

      nfc_15 <- dataset() |>
        dplyr::filter(nfc_intake_cum <= 16) |>
        dplyr::pull(nfc_intake_cum) |>
        length()

      dataset() |>
        dplyr::summarise(
          final_body_weight = dplyr::last(BWcor), #input$`teste_1-input_teste`,
          aver_daily_gain = ( final_body_weight - min(BW) ) / dim(dataset())[1],
          starter_intake = sum(starterIntake, na.rm = TRUE) / dim(dataset())[1],
          milkME = mean(MEfromliqDiet / liqDietIntake, na.rm = TRUE),
          nfcCum = nfc_15
        )

    })

    #output$teste <- renderTable({

      # dados$dados_teste |> print()
      #

      #kpis_calculations()["final_body_weight"]

    #})

    output$fBW <- renderText({

      kpis_calculations() |>
        dplyr::pull(final_body_weight) |>
        round(2)

    })

    output$ADG <- renderText({

      kpis_calculations() |>
        dplyr::pull(aver_daily_gain) |>
        round(2)

    })

    output$ST <- renderText({

      kpis_calculations() |>
        dplyr::pull(starter_intake) |>
        round(2)

    })

    output$milkME <- renderText({

      kpis_calculations() |>
        dplyr::pull(milkME) |>
        round(2)

    })

    output$nfcCum <- renderText({

      kpis_calculations() |>
        dplyr::pull(nfcCum) |>
        round(0)

    })

  })
}
