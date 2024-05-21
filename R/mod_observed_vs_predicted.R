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
        class = "bg-dark",
        "Observed vs Predicted."),
      height = 800,
      bslib::card_body(

        plotly::plotlyOutput(ns("plotObservedPredicted"), height = 600, width = "50%"),

        tableOutput(ns("modelEvaluationMetrics")),

        fileInput(ns("observedFile"), "Choose xlsx file",
                  accept = c(".xlsx")),

        fluidRow(
          column(offset = 10, 2,
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

    # dropbox access

    token <- readRDS("inst/app/rdrop/droptoken.rds")

    new_token <- token$refresh()

    saveRDS(new_token, "inst/app/rdrop/droptoken.rds")

    dataUploaded <- reactive({

      inFile <- input$observedFile

      if (is.null(inFile))
        return(NULL)

      readxl::read_excel(inFile$datapath)

    })


    templateFile <- readxl::read_xlsx("data-raw/dataEntryTemplateCalfSim.xlsx")

    predicoes <- reactive({

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
          liq_diet          = rep(5, 70),
          liq_diet_me        = 4.6,
          starter_composition = list(
            cs_ndf = 12,
            cs_nfc = 50,
            cs_cp = 22,
            cs_ee = 6,
            form_of_starter = "pelleted"
          ),
          liq_diet_dm        = 0.12,
          initial_bw           = bezerros[[i]]$initial_bw,
          weaning_age       = 70,
          average_temperature         = 20,
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
            y = "Observed Body Weight (kg)"
          )
      )

    })

    output$modelEvaluationMetrics <- renderTable({

      nObs <- length(predicoes()$body_weight_kg)

      model_eval(predicoes()$body_weight_kg, predicoes()$BW_cor) |>
        dplyr::mutate(
          meanPred = mean(predicoes()$BW_cor, na.rm = TRUE),
          meanObs = mean(predicoes()$body_weight_kg, na.rm = TRUE),
          nObs = nObs
        ) |>
        dplyr::relocate(
          meanPred, meanObs, nObs, .before = "P-value t test"
        )

    })

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
