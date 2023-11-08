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
        min_height = 500,
        uiOutput(ns("var_selection")),
        plotly::plotlyOutput(ns("dash_plot"))
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
                 selectInput(ns("var_1"), "Sel. X axis Var:", choices = names(dataset()), selected = names(dataset())[1])),
          column(4,
                 selectInput(ns("var_2"), "Sel. 1° Y axis Var (left):", names(dataset()), selected = names(dataset())[6])),
          column(4,
                 selectInput(ns("var_3"), "Sel. 2° Y axis Var (right):", names(dataset()), selected = names(dataset())[13]))
        )
      )
    })

    output$dash_plot <- plotly::renderPlotly({

      req(input$var_1)

      line_plotly(data       = dataset(),
                  x_axis     = input$var_1,
                  y_axis     = input$var_2,
                  x_label    = input$var_1,
                  y_label    = input$var_2,
                  y_axis_2   = input$var_3,
                  y_axis_2_label = input$var_3,
                  plot_title = paste("Relationship between", input$var_1, "and", input$var_2))

    })

  })
}
