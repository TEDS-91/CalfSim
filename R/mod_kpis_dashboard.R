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

    # Creating the dynamic dashboard ui

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "KPI's",
        class = "bg-green"
      ),
      bslib::card_body(
        uiOutput(ns("kpis_dashboard_ui"))
      )
    )
  )
}

#' kpis_dashboard Server Functions
#'
#' @noRd
mod_kpis_dashboard_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# -------------------------------------------------------------------------
# Getting the number of scenarios -----------------------------------------
# -------------------------------------------------------------------------
    number_of_scenarios <- reactive({

      dataset() |>
      dplyr::select(scenario) |>
        unique() |>
        nrow()
    })

# -------------------------------------------------------------------------
# KPIs calculations based on the scenarios simulated ----------------------
# -------------------------------------------------------------------------

    kpis_calculations <- reactive({

      age_nfc_15 <- dataset() |>
        dplyr::group_by(scenario) |>
        dplyr::filter(nfc_intake_cum < 16) |>
        dplyr::summarise(
          age_nfc_15 = max(days_of_life, na.rm = TRUE),
          .groups = "drop"
        )

      kpis <- dataset() |>
        dplyr::group_by(scenario, weaned) |>
        dplyr::summarise(
          nobs = dplyr::n(),
          final_body_weight = round(dplyr::last(BW_cor, na_rm = TRUE), 1),
          aver_daily_gain = round(mean(ADG, na.rm = TRUE), 3),
          starter_intake = round(sum(starter_intake, na.rm = TRUE) / nobs, 3),
          milkME = round(mean(me_from_liq_diet / liq_diet_intake, na.rm = TRUE), 3),
          "Total Milk Consumption (kg)" = round(sum(liq_diet_all), 2),
          "Total Starter Consumption (kg)" = round(sum(starter_intake), 2),
          "Total Milk Cost ($)" = round(sum(total_milk_cost), 2),
          "Total Starter Cost ($)" = round(sum(total_starter_cost), 2),
          "Total Feed Cost ($)" = round(sum(total_milk_cost + total_starter_cost), 2),
          "Cost per kg of BW gain ($)" = round(`Total Feed Cost ($)` / (max(BW_cor) - min(BW)), 2)
        ) |>
        dplyr::ungroup() |>
        dplyr::left_join(age_nfc_15, by = "scenario") |>
        dplyr::filter(weaned == FALSE) |>
        dplyr::group_split(scenario)

    })

# -------------------------------------------------------------------------
# Function to use in the KPI display --------------------------------------
# -------------------------------------------------------------------------

    # function for the value boxes

    # small function for the value boxes

    value_boxes_adj <- function(variable = "Body weight (kg)",
                                alias = "fBW",
                                icon = "graph-up", ...) {

      list(
        column(3,
               bslib::value_box(
                 title = variable,
                 value = alias,
                 showcase = bsicons::bs_icon(icon),
                 showcase_layout = "top right"
               ))
      )
    }

    # function to display the value boxes according to the number of scenarios simulated

    scenario_visual <- function(scenario_id, metrics = list(body_weight, adg, age_nfc_15, me_milk)) {

      tagList(
        strong(scenario_id),
        bslib::layout_column_wrap(
          width = "250px",
          height = "130px",
          fluidRow(
            data.frame(variable = c("Final Body Weight (kg)", "ADG (kg)", "15 kg of NFCI (days)", "Cost/kg of BW gain ($)"),
                       alias = c(metrics$body_weight, metrics$adg, metrics$age_nfc_15, metrics$me_milk),
                       icon = c("graph-up", "graph-up", "bucket", "currency-dollar")) |>
              purrr::pmap(value_boxes_adj)
          )
        )
      )
    }

# -------------------------------------------------------------------------
# Building the UI ---------------------------------------------------------
# -------------------------------------------------------------------------

    output$kpis_dashboard_ui <- renderUI({

      seq(1, number_of_scenarios(), 1) |>
        lapply(\(x)
              scenario_visual(scenario_id = kpis_calculations()[[x]]$scenario[1],
                              metrics = list(body_weight = kpis_calculations()[[x]]$final_body_weight,
                                             adg = kpis_calculations()[[x]]$aver_daily_gain,
                                             age_nfc_15 = kpis_calculations()[[x]]$age_nfc_15,
                                             me_milk = kpis_calculations()[[x]]$`Cost per kg of BW gain ($)`)))
    })

  })
}
