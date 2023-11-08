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

    bslib::value_box(
      title = "Final Body weight (kg)",
      value = textOutput(ns("fBW")),
      showcase = bsicons::bs_icon("ev-station-fill")
    ),

    tableOutput(ns("Teste"))
  )
}

#' kpis_dashboard Server Functions
#'
#' @noRd
mod_kpis_dashboard_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    kpis_calculations <- reactive({


      dataset() |>
        dplyr::summarise(
          final_body_weight = dplyr::last(BWcor),
          aver_daily_gain = ( final_body_weight - min(BW) ) / dim(dataset())[1]
        )

    })

    output$Teste <- renderTable({

      kpis_calculations()["final_body_weight"]

    })

    output$fBW <- renderText({

      kpis_calculations() |>
        dplyr::pull(final_body_weight) |>
        round(2)

    })

  })
}
