#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  requirements_dataset <- mod_inputs_page_server("requirements_calculations")

  mod_data_table_server("dataSimulated", dataset = requirements_dataset[["data_frame_simulated"]])

  mod_dashboard_plot_server("plotDashboard", dataset = requirements_dataset[["data_frame_simulated"]])

  mod_kpis_dashboard_server("key_performance_indicators", dataset = requirements_dataset[["data_frame_simulated"]])

  mod_descriptive_pre_weaning_performance_server("descriptive_pre_weaning_performance", dataset = requirements_dataset[["data_frame_simulated"]])

  mod_nutrient_requirements_server("NASEM")

}
