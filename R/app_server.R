#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # initial message

  shinyalert::shinyalert(title = "Welcome to CalfSim Model! ",
                         text =
                         "
                         CalfSim is currently under development and may frequently change.

                         To use CalfSim, enter your inputs and click 'Simulate'.
                         You'll be directed to the Dashboard page with the results.

                         The On-farm Assessment page is still in development.

                         For feedback and questions, contact Dr. Tadeu da Silva
                         at tdasilva@uvm.edu.

                         ",
                         type = "info",
                         size = "s")

  requirements_dataset <- mod_inputs_page_server("requirements_calculations",
                                                 simulate_button = reactive(input$simulate_button))

  observeEvent(input$simulate_button, {
    updateTabsetPanel(session, "nav", "Dashboard")
    session$sendCustomMessage(type = 'scrollToTop', message = 'dummy')
  })


  mod_all_dynamic_scenarios_server("all_dynamic_scenarios_1")


  mod_data_table_server("dataSimulated",
                        dataset = requirements_dataset[["data_frame_simulated"]])

  mod_dashboard_plot_server("plotDashboard",
                            dataset = requirements_dataset[["data_frame_simulated"]])

  mod_kpis_dashboard_server("key_performance_indicators",
                            dataset = requirements_dataset[["data_frame_simulated"]])

  mod_descriptive_pre_weaning_performance_server("descriptive_pre_weaning_performance",
                                                 dataset = requirements_dataset[["data_frame_simulated"]])

  mod_observed_vs_predicted_server("observed_vs_predicted")

  mod_nutrient_requirements_server("NASEM")

  mod_mod_calfsim_info_server("mod_calfsim_info_1")


}
