#' descriptive_pre_weaning_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_descriptive_pre_weaning_performance_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Table with all data simulated."),
      height = 800,
      bslib::card_body(
        min_height = 500,
        DT::DTOutput(ns("table")),
      )
    ),

  hr(),

  fluidRow(column(offset = 10, 2, downloadButton(ns("downloadData"), label = "Download!")),
           column(offset = 10, 2, downloadButton(ns("downloadreport"), label = "Report!")))

  )
}

#' descriptive_pre_weaning_performance Server Functions
#'
#' @noRd
mod_descriptive_pre_weaning_performance_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    descriptive_data <- reactive({

      dataset() |>
        dplyr::group_by(scenario) |>
        dplyr::filter(daysOfLife <=56) |>
        dplyr::summarise(
          "Initial Body Weight (kg)" = dplyr::first(BW),
          "Final Body Weight (kg)" = dplyr::last(BWcor),
          "Av. Daily Gain (kg)" = (dplyr::last(BWcor) - dplyr::first(BW)) / max(daysOfLife),
          "Av. Daily Feed Intake (kg)" = sum(totalDMI) / max(daysOfLife),
          "Feed Efic. (kg/kg)" = sum(totalDMI) / (dplyr::last(BWcor) - dplyr::first(BW)),
          "Age at Weaning (days)" = max(daysOfLife),
          "Total Milk Consumption (kg)" = sum(LiqDietAll)
        ) |>
        tidyr::pivot_longer(
          cols = -scenario,
          names_to = "Variable",
          values_to = "Value"
        ) |>
        tidyr::pivot_wider(
          names_from = scenario,
          values_from = Value
        ) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

    })



    output$table <- DT::renderDT({

      descriptive_data()

    }, options = list(paging = TRUE,
                      scrollY = "600px",
                      scrollX = "900px",
                      pageLength = 20,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#007582', 'color': '#fff'});",
                        "}")),
    style = "bootstrap5")

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("PreWeaningPerformance", ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(descriptive_data(), path = file)
      }
    )


    # report generation

    output$downloadreport <-

      downloadHandler(
        "CalfSimReport.html",
        content =
          function(file)
          {

            withProgress(message = "Rendering the report...", {

            path_reportRMD <- system.file("app", "report.Rmd", package = "CalfSim")

            path_reportHTML <- system.file("app", "built_report.html", package = "CalfSim")

            rmarkdown::render(
              input = path_reportRMD,
              output_file = "built_report.html",

              params = list(
                teste = descriptive_data(),
                all_data = dataset()
              )
            )

            readBin(con = path_reportHTML,
                    what = "raw",
                    n = file.info(path_reportHTML)[ , "size"]) |>

              writeBin(con = file)

          })
          }
      )

  })
}

## To be copied in the UI
# mod_descriptive_pre_weaning_performance_ui("descriptive_pre_weaning_performance_1")

## To be copied in the server
# mod_descriptive_pre_weaning_performance_server("descriptive_pre_weaning_performance_1")
