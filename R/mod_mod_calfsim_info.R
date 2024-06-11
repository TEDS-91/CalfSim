#' mod_calfsim_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_calfsim_info_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        "CalfSim Model Information",
        class = "bg-green"
      ),
      bslib::card_body(
        "This is a model to simulate the growth of calves from birth to weaning.
        The model is based on the NASEM (2016) model for nutrient requirements and
        the NRC (2001) model for growth. The model is implemented in R and Shiny and is available at
        ....")),

    bslib::layout_column_wrap(
      width = 1/2,
      height = 300,
      bslib::card(full_screen = FALSE,
                  bslib::card_header("Tadeu Eder da Silva, PhD.",
                                     class = "bg-green"),
                  bslib::card_body(
                    bslib::layout_column_wrap(
                      bslib::card(full_screen = FALSE,
                                  bslib::card_body(
                                    div(style = "text-align: center;",
                                    img(src = "www/tadeu_photo.jpeg",
                                        width = 200,
                                        height = 200,
                                        style = "border-radius: 20%; ")))),
                      bslib::card(full_screen = FALSE,
                                  width = 2/3,
                                  bslib::card_header("Contact"),
                                  bslib::card_body(
                                    htmltools::HTML(paste("Email:",
                                                          "tdasilva@uvm.edu/ tadeuederzootecnista@gmail.com",
                                                          tags$a(href = "https://github.com/TEDS-91", "TEDS-91 (github page)"),
                                                          sep = "<br/>"))
                                    ))

                    )
                  )),
      bslib::card(full_screen = FALSE,
                  class = "bg-grey",
                  bslib::card_header("Joao Costa, PhD.",
                                     class = "bg-green"),
                  bslib::card_body(
                    bslib::layout_column_wrap(
                      bslib::card(full_screen = FALSE,
                                  bslib::card_body(
                                    div(style = "text-align: center;",
                                    img(src = "www/joao_photo.jpeg",
                                        width = 200,
                                        height = 200,
                                        style = "border-radius: 20%; ")))),
                      bslib::card(full_screen = FALSE,
                                  width = 2/3,
                                  bslib::card_header("Contact"),
                                  bslib::card_body(
                                    htmltools::HTML(paste("Email:",
                                                          "jcardoso@uvm.edu/ jhcardosocosta@gmail.com",
                                                          tags$a(href = "https://costalab.weebly.com/", "Costa's Lab Web Page!"),
                                                          sep = "<br/>"))
                      ))
      ))))

  )
}

#' mod_calfsim_info Server Functions
#'
#' @noRd
mod_mod_calfsim_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_mod_calfsim_info_ui("mod_calfsim_info_1")

## To be copied in the server
# mod_mod_calfsim_info_server("mod_calfsim_info_1")
