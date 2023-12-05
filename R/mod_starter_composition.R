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

    DT::DTOutput(ns("tabela")),
    tableOutput(ns("tabela_atual")),
    actionButton(ns("deleteRows"), "Delete Rows"),
    actionButton(ns("addRows"), "Add Rows")

  )
}

#' starter_composition Server Functions
#'
#' @noRd
mod_starter_composition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df <- tibble::tibble(
      "Ingredient" = c("Soyben Meal", "Corn Grain", "Corn Gluten Meal", "Wheat Bran", "Soybean Oil"),
      "Dry Matter (%)" = c(89, 89, 89, 89, 1),
      "Crude Protein (%)" = c(48, 9, 4, 15, 0),
      "Metab. Energy (Mcal/kg)" = c(3.2, 3.2, 3.2, 3.2, 8.9),
      "Diet Prop. (%)" = c(25, 70, 0, 0, 0)
    )

    tabela_para_salvar <- reactiveVal(NULL)

    output$tabela <- DT::renderDataTable({

      tab_mtcars <- df

      tabela_para_salvar(tab_mtcars)

      tab_mtcars |>
        DT::datatable(
          editable = TRUE
        )
    })

    proxy <- DT::dataTableProxy("tabela")


    observeEvent(input$addRows, {

      tab_atual <- tabela_para_salvar()

      tab_atualizada <- tab_atual |>
        dplyr::add_row("Ingredient" = "Added Feed",
                       "Dry Matter (%)" = NA,
                       "Crude Protein (%)" = NA,
                       "Metab. Energy (Mcal/kg)" = NA,
                       "Diet Prop. (%)" = 0)

      DT::replaceData(
        proxy,
        tab_atualizada
      )

      tabela_para_salvar(tab_atualizada)

    })

    observeEvent(input$deleteRows, {

      tab_atual <- tabela_para_salvar()

      if(is.null(input$tabela_rows_selected)){

        tabela_para_salvar(tab_atual)

      } else {

        tab_atualizada <- tab_atual[-as.numeric(input$tabela_rows_selected), ]

        DT::replaceData(
          proxy,
          tab_atualizada
        )

        tabela_para_salvar(tab_atualizada)
      }

    })

    observeEvent(input$tabela_cell_edit, {

      tab_atual <- tabela_para_salvar()

      tab_atualizada <- DT::editData(
        tab_atual,
        input$tabela_cell_edit
      )

      DT::replaceData(
        proxy,
        tab_atualizada
      )

      tabela_para_salvar(tab_atualizada)

    })

    teste <- reactive({

      tabela_para_salvar() |>
        dplyr::summarise(
          "ME (Mcal/kg)" = sum(`Metab. Energy (Mcal/kg)` * `Diet Prop. (%)` / 100, na.rm = TRUE),
        )
    })

    output$tabela_atual <- renderTable({
      teste()
    })



  })
}

## To be copied in the UI
# mod_starter_composition_ui("starter_composition_1")

## To be copied in the server
# mod_starter_composition_server("starter_composition_1")
