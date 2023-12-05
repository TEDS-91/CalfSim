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
        class = "bg-dark",
        "Select the variables to be plotted."
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

    output$var_selection <- renderUI({

      tagList(
        fluidRow(
          column(4,
                 selectInput(ns("var_1"), "Sel. X axis Var:", choices = names(dataset()), selected = names(dataset())[2])),
          column(4,
                 selectInput(ns("var_2"), "Sel. 1° Y axis Var (left):", names(dataset()), selected = names(dataset())[7])),
          column(4,
                 selectInput(ns("var_3"), "Sel. 2° Y axis Var (right):", names(dataset()), selected = names(dataset())[14]))
        )
      )
    })

    output$echarts_plot1 <- echarts4r::renderEcharts4r({

      req(input$var_1)

      plot1 <- scenario_plot(dataset(), input$var_2)

      plot1

    })

    output$echarts_plot2 <- echarts4r::renderEcharts4r({

      req(input$var_1)

      plot2 <- scenario_plot(dataset(), input$var_3)

      plot2

    })

  })
}
