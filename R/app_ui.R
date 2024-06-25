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

    tags$head(
      tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo(0, 0);
      });
    "))
    ),

    bslib::page_navbar(
      title = div(img(src = "www/calfsimlogo.png", height = "70px", width = "100px"), ""),
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
        value = "Inputs",
        mod_inputs_page_ui("requirements_calculations"),

        fluidRow(
          column(offset = 10, width = 2,
                 actionButton("simulate_button",
                              label = "Simulate",
                              icon = tags$i(fontawesome::fa("person-running")),
                              style = "color: white; background-color: #036c15; border-color: #036c15") |>
                   bslib::tooltip(
                     paste("Simulate the calf growth based on the inputs provided.", "\n",
                     "Hitting this button is mandatory to get the simulation outcomes.")
                   )
          )
        )#,
        #mod_data_table_ui("dataSimulated")
      ),
      bslib::nav_panel(
        title = "Dashboard",
        value = "Dashboard",
        mod_kpis_dashboard_ui("key_performance_indicators"),
        mod_dashboard_plot_ui("plotDashboard"),
        mod_descriptive_pre_weaning_performance_ui("descriptive_pre_weaning_performance")
      ),
      bslib::nav_panel(
        title = "On-farm Assessment",
        mod_observed_vs_predicted_ui("observed_vs_predicted")
      ),
      # bslib::nav_panel(
      #   title = "Nutrient Requirements NASEM (2021)",
      #   mod_nutrient_requirements_ui("NASEM")
      # ),
      bslib::nav_panel(
        title = "About CalfSim",
        mod_mod_calfsim_info_ui("mod_calfsim_info_1")
      )
    ),
    # This will add a logo at the bottom across all tabs
    hr(),
    tags$footer(
      div(style = "text-align: center; padding: 10px 0;",
          img(src = "www/logolab.png", height = "120px", width = "150px"),
      ))
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
    favicon(ico = "bezerro"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CalfSim"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
