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
      bg = "#66AC47",
      id = "nav",
      fillable_mobile = TRUE,
      fluid = TRUE,
      fillable = FALSE,
      theme = bslib::bs_theme(version = 5),

      sidebar = bslib::sidebar(

      ),

      bslib::nav_panel(
        title = "fistPage"

      ),
      bslib::nav_panel(
        title = "secondPage"

      ),
      bslib::nav_panel(
        title = "thirdPage"
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
