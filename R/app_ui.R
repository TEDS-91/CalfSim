#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    bslib::page_navbar(

      title = "CalfSim",
      #bg = "#66AC47",
      id = "nav",
      fillable_mobile = TRUE,
      fluid = TRUE,
      fillable = FALSE,
      theme = bslib::bs_theme(version = 5),
      # sidebar = bslib::sidebar(
      # ),
      bslib::nav_panel(
        title = "Inputs",

        mod_inputs_page_ui("requirements_calculations"),
        mod_data_table_ui("dataSimulated")

        # numericInput("tt1", "tt1", 2),
        # numericInput("tt2", "tt2", 3),
        # numericInput("tt3", "tt3", 15),
        #
        # list("teste1", "teste2", "teste3") |>
        #   lapply(card_title = "testes", valueBoxCustom)
      ),
      bslib::nav_panel(
        title = "Dashboard",
        mod_kpis_dashboard_ui("key_performance_indicators"),
        mod_dashboard_plot_ui("plotDashboard"),
        mod_descriptive_pre_weaning_performance_ui("descriptive_pre_weaning_performance")
      ),
      bslib::nav_panel(
        title = "Nutrient Requirements NASEM (2021)",
        mod_nutrient_requirements_ui("NASEM")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CalfSim"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
