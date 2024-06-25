#' observed_vs_predicted UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_observed_vs_predicted_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-green",
        "Animal, Management, Environmental, and Liquid Diet Inputs."),
      bslib::layout_column_wrap(
        width = NULL,
        height = 270,
        style = htmltools::css(grid_template_columns = "1.3fr 2fr"),
        bslib::card(
          fluidRow(
            column(6,
                   numericInput(ns("temp"), label = h6(strong("Aver. Temp. (C):")), value = 15)),
            column(6,
                   numericInput(ns("weaning_age"), label = h6(strong("Weaning Age (days):")), value = 56)))),
        bslib::card(
          mod_milk_composition_ui(ns("milk_milk_replacer_composition")))
      )
    ),
    mod_starter_composition_ui(ns("starter_composition")),
    bslib::card(
      bslib::card_header(
        class = "bg-green",
        "Scenarios for Milk Allowance Plans."),
      mod_dynamic_scenarios_ui(ns("on_farm_scenario"), scenario_name = "On Farm Data Scenario"),
      fluidRow(
        column(offset = 7, width = 3,
               fileInput(ns("observedFile"), "Choose xlsx file",
                         accept = c(".xlsx"))
        ),
        column(width = 2,
               actionButton(ns("simulate_button"),
                            label = "Simulate",
                            icon = tags$i(fontawesome::fa("person-running")),
                            style = "color: white; background-color: #036c15; border-color: #036c15")
        )
      ),
    ),
    textOutput(ns("data_preparation")),
    bslib::card(
      bslib::card_header(
        class = "bg-green",
        "Observed vs Predicted."),
      height = 800,
      bslib::card_body(

        bslib::layout_column_wrap(

        plotly::plotlyOutput(ns("plotObservedPredicted"), height = 600),

        DT::DTOutput(ns("modelEvaluationMetrics"))),

        fluidRow(
          column(offset = 9, 3,
                 downloadButton(ns("templateFile"),
                                "Download Template!"))
        )
      )
    )

  )
}

#' observed_vs_predicted Server Functions
#'
#' @noRd
mod_observed_vs_predicted_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # dynamic scenarios

    on_farm_feeding_plan <- reactive({

      on_farm_feeding_plan <- mod_dynamic_scenarios_server("on_farm_scenario")

    })

    on_farm_feeding_plan_to_populate_get_requirements <- reactive({

      on_farm_feeding_plan_to_populate_get_requirements <- milk_allowance_program(weaning_age       = input$weaning_age,
                                                                                  nutritional_plans = on_farm_feeding_plan()()[["nutritional_plans"]],
                                                                                  milk_allowance    = on_farm_feeding_plan()()[["milk_allowance"]],
                                                                                  ages_of_change    = on_farm_feeding_plan()()[["days_of_change"]] )

    })

    output$data_preparation <- renderText({

      on_farm_feeding_plan()
      print(" ")

    })

    # starter composition

    starter_composition <- mod_starter_composition_server("starter_composition")

    # milk composition

    milk_composition <- mod_milk_composition_server("milk_milk_replacer_composition")

    # dropbox access

    #token <- readRDS("inst/app/rdrop/droptoken.rds")

    #new_token <- token$refresh()

    #saveRDS(new_token, "inst/app/rdrop/droptoken.rds")

    dataUploaded <- reactive({

      inFile <- input$observedFile

      if (is.null(inFile))
        return(NULL)

      readxl::read_excel(inFile$datapath)

    })


    path_template <- system.file("app", "dataEntryTemplateCalfSim.xlsx",
                         package = "CalfSim")

    templateFile <- readxl::read_xlsx(path = path_template)

    predicoes <- eventReactive(input$simulate_button, {

      if(is.null(dataUploaded())) {

        bezerros <- templateFile

      } else {

        bezerros <- dataUploaded()

      }

      bezerros <- bezerros |>
        dplyr::rename(initial_bw = birth_weight_kg) |>
        dplyr::group_split(animal)

      predictions <- list()

      for (i in 1:length(bezerros)) {

        predictions[[i]] <- get_calf_requirements(
          liq_diet          = on_farm_feeding_plan_to_populate_get_requirements(),
          liq_diet_me        = milk_composition()[["milk_ME"]],
          starter_composition = list(
            cs_ndf = starter_composition()[["cs_ndf"]],
            cs_nfc = starter_composition()[["cs_nfc"]],
            cs_cp = starter_composition()[["cs_cp"]],
            cs_ee = starter_composition()[["cs_ee"]],
            form_of_starter = starter_composition()[["form_of_starter"]]
          ),
          liq_diet_dm        = milk_composition()[["liq_diet_dm"]],
          initial_bw           = bezerros[[i]]$initial_bw,
          weaning_age       = input$weaning_age,
          average_temperature         = input$temp,
          liq_diet_only      = FALSE,
          mature_weight    = 750,
          max_size         = 100
        )
      }

      valores_pred <- list()

      for (i in 1:length(predictions)) {

        valores_pred[[i]] <- predictions[[i]] |>
          dplyr::filter(
            days_of_life == bezerros[[i]]$age_days
          ) |>
          dplyr::select(
            days_of_life, BW_cor
          )

      }

      dfComplete <- valores_pred |>
        dplyr::bind_rows(.id = "animal") |>
        dplyr::left_join(bezerros |> dplyr::bind_rows(.id ="animal"),
                         by = "animal")

      dfComplete

    })

    output$plotObservedPredicted <- plotly::renderPlotly({

      plotly::ggplotly(
        predicoes() |>
          dplyr::mutate(animal = as.factor(animal)) |>
          dplyr::rename(predicted_bw = BW_cor, observed_bw = body_weight_kg) |>
          ggplot2::ggplot(ggplot2::aes(x = predicted_bw, y = observed_bw)) +
          ggplot2::theme_bw() +
          ggplot2::geom_point(size = 2, alpha = 0.5) +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
          ggplot2::geom_smooth(method = "lm", se = TRUE) +
          ggplot2::labs(
            x = "Predicted Body Weight (kg)",
            y = "Observed Body Weight (kg)",
            caption = "Note: Solid red line represents X = Y."
          )
      )

    })

    output$modelEvaluationMetrics <- DT::renderDT({

      nObs <- length(predicoes()$body_weight_kg)

      model_eval(predicoes()$body_weight_kg, predicoes()$BW_cor) |>
        dplyr::mutate(
          meanPred = mean(predicoes()$BW_cor, na.rm = TRUE),
          meanObs = mean(predicoes()$body_weight_kg, na.rm = TRUE),
          nObs = nObs
        ) |>
        dplyr::relocate(
          meanPred, meanObs, nObs, .before = "P-value t test"
        ) |>
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "Stats",
          values_to = "Value"
        ) |>
        dplyr::mutate(
          Value = round(Value, 2)
        )

    }, options = list(paging = TRUE,
                      scrollY = "400px",
                      scrollX = "900px",
                      pageLength = 10,
                      rownames = FALSE,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#007582', 'color': '#fff'});",
                        "}")),
    style = "bootstrap5")

    output$templateFile <- downloadHandler(

      filename = function() {

        paste("dataEntryTemplateCalfSim", ".xlsx")

      },

      content = function(file) {

        writexl::write_xlsx(templateFile, path = file)

      }
    )

    # Saving the data into DropBox

    observeEvent(input$observedFile, {

      saveData <- function(data, path, token) {

        # Create a unique file name
        fileName <- paste0("dataset_uploaded_date_and_time_",
                           gsub(":", "_", gsub("-", "_", gsub(" ", "_", tolower(Sys.time())))),
                           ".csv")

        # Write the data to a temporary file locally
        filePath <- file.path(tempdir(), fileName)
        utils::write.csv(data, filePath, row.names = FALSE, quote = TRUE)

        # Upload the file to Dropbox
        rdrop2::drop_upload(filePath, path = path, dtoken = token)

      }

      saveData(dataUploaded(),
               token = new_token,
               path = "calfsimdata")

    })

  })
}
