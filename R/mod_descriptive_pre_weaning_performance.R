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
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Performance until weaning."),
      height = 900,
      bslib::card_body(
        min_height = 850,
        DT::DTOutput(ns("table")),
      )
    ),

  hr(),

  fluidRow(column(offset = 8, 2, downloadButton(ns("downloadData"), label = "Download!")),
           column(2, downloadButton(ns("downloadreport"), label = "Report!")))

  )
}

#' descriptive_pre_weaning_performance Server Functions
#'
#' @noRd
mod_descriptive_pre_weaning_performance_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    descriptive_data <- reactive({

      age_nfc_15 <- dataset() |>
        dplyr::group_by(scenario) |>
        dplyr::filter(nfc_intake_cum < 17) |>
        dplyr::summarise(
          age_nfc_15 = max(days_of_life, na.rm = TRUE),
          me_cs = round(max(me_calf_starter, na.rm = TRUE), 2),
          .groups = "drop"
        )

      dataset() |>
        dplyr::filter(weaned == FALSE) |>
        dplyr::group_by(scenario) |>
        dplyr::summarise(
          "Initial Body Weight (kg)" = round(dplyr::first(BW), 2),
          "Final Body Weight (kg)" = round(dplyr::last(BW_cor), 2),
          "Av. Daily Gain (kg)" = round(mean(ADG, na.rm = TRUE), 3),
          "Av. Daily Feed Intake (kg)" = round(mean(total_dmi, na.rm = TRUE), 3),
          "Feed Efic. (kg/kg)" = round(sum(total_dmi) / (dplyr::last(BW_cor) - dplyr::first(BW)), 3),
          "Age at Weaning (days)" = max(days_of_life),
          "Total Milk Consumption (kg)" = round(sum(liq_diet_all), 2),
          "Total Starter Consumption (kg)" = round(sum(starter_intake), 2),
          "Total Milk Cost ($)" = round(sum(total_milk_cost), 2),
          "Total Starter Cost ($)" = round(sum(total_starter_cost), 2),
          "Total Feed Cost ($)" = round(sum(total_milk_cost + total_starter_cost), 2),
          "Cost per kg of BW gain ($)" = round(`Total Feed Cost ($)` / (max(BW_cor) - min(BW)), 2)
        ) |>
        dplyr::left_join(age_nfc_15, by = "scenario") |>
        dplyr::rename("Age at 15 kg of NFC intake (days)" = age_nfc_15,
                      "Starter Met. Energy (Mcal)" = me_cs) |>
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
