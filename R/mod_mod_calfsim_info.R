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
        "CalfSim Model Summary",
        class = "bg-green"
      ),
      bslib::card_body(
        "
        The CalfSim Tool is a free and user-friendly tool that aims to allow
        consultants and researchers to simulate calf performance under different
        nutritional plans. As basis for developing the tool, the equations were
        used for calf starter intake and nutritional requirements presented in
        the National Academies of Sciences, Engineering, and Medicine
        (NASEM, 2021). In determining the metabolizable energy (ME; Mcal)
        of the calf starter, in addition to its chemical composition, we used
        the cumulative intake of non-fibrous carbohydrates (NFC) as an indicator
        of gastrointestinal tract maturity. The maximum calf starter ME value was
        achieved with an accumulated NFC consumption of 15 kg (Quigley, 2019). We
        developed the CalfSim Tool using the package and framework for web
        application development called Shiny as a backbone, available in the R
        rogramming language (R Core Team version 4.2.0). In utilizing the
        CalfSim Tool, the user is required to input information regarding calf
        initial weight (kg), weaning age (d), average temperature (°C),
        nutritional composition of whole milk or milk replacer, and the number
        of scenarios to be simulated. Within each scenario, the user has the
        flexibility to specify the daily quantity of milk or milk replacer
        provided to the calves. The user must enter the nutritional composition
        of the calf starter to estimate its ME value. After entering the
        information, the user must run the model and analyze the outcomes
        summarized in a dashboard, comparing each simulated scenario in
        terms of key performance indicators, such as final weight (kg),
        average daily gain (kg), and age at accumulated consumption of 15
        kg NFC.

        ")),

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
                                                          tags$a(href = "https://github.com/TEDS-91", "TEDS-91 (github page)",
                                                                 target = "_blank"),
                                                          sep = "<br/>"))))
                      )
                    )
                  ),
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
                                                          "joao.costa@uvm.edu/ jhcardosocosta@gmail.com",
                                                          tags$a(href = "https://costalab.weebly.com/", "Costa's Lab Web Page!",
                                                                 target = "_blank"),
                                                          sep = "<br/>"))))
                      )
                    )
                  )
      ),

    bslib::card(
      bslib::card_header(
        "Acknowledgements and sponsors",
        class = "bg-green"
      ),
      bslib::card_body(
        htmltools::HTML(paste(
          "We would like to thank the University of Vermont (UVM) for providing
          the resources and support to develop the CalfSim Tool and the
          the UVM's Food Systems Research Center (FSRC) for supporting this project.",

          sep = "<br/>"
        )
      ),
      div(style = "text-align: center;",
          img(src = "www/sponsor_wanted.jpeg",
              width = 250,
              height = 250,
              style = "border-radius: 20%; "))
    )
  ),

    bslib::card(
      bslib::card_header(
        "References",
        class = "bg-green"
      ),
      bslib::card_body(
        htmltools::HTML(paste(

          "National Academies of Sciences, Engineering, and Medicine (NASEM). 2021.
          Nutrient Requirements of Dairy Cattle: Eighth Revised Edition. Washington,
          DC: The National Academies Press.",

          " ",

          "Quigley, J. D., W. Hu, J. R. Knapp, T. S. Dennis, F. X. Suarez-Mena,
          and T. M. Hill. 2019a. Estimates of calf starter energy affected by
          consumption of nutrients. 1. Evaluation of models to predict changing
          digestion on energy content in calf starters. J. Dairy Sci. 102:2232–2241.",

          " ",

          "Quigley, J. D., W. Hu, J. R. Knapp, T. S. Dennis, F. X. Suarez-Mena,
          and T. M. Hill. 2019b. Estimates of calf starter energy affected
          by consumption of nutrients. 2. Effect of changing digestion on
          energy content in calf starters. J. Dairy Sci. 102:2242–2253.",

          " ",

          "R Core Team. 2021. R: A language and environment for statistical computing.
          R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.",

          sep = "<br/>"
        )
      )
    )
  )
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
