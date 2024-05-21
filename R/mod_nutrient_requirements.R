#' nutrient_requirements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nutrient_requirements_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::nav_panel(

      title = "NASEM (2021)",

      bslib::layout_column_wrap(
        width = 1/2,
        height = 180,

        bslib::card(
          bslib::card_header(class = "bg-dark", "Weight"),
          bslib::card_body(
            fluidRow(
              column(4,
                     numericInput(ns("min_peso"), "Min. Weight", 30)),
              column(4,
                     numericInput(ns("max_peso"), "Max. Weight", 60)),
              column(4,
                     numericInput(ns("int_peso"), "Inter. Weight", 10))
            )
          )
        ),

        bslib::card(
          bslib::card_header(class = "bg-dark", "ADG"),
          bslib::card_body(
            fluidRow(
              column(4,
                     numericInput(ns("min_adg"), "Min. ADG", 0.1)),
              column(4,
                     numericInput(ns("max_adg"), "Max. ADG", 0.9)),
              column(4,
                     numericInput(ns("int_adg"), "Inter. ADG", 0.2))
            )
          )
        )
      ),

      bslib::card(
        bslib::card_header(class = "bg-dark", "Requeriments..."),
        height = 800,
        bslib::card_body(
          min_height = 500,
          DT::dataTableOutput(ns("NASEM"))
        )
      )
    )

  )
}

#' nutrient_requirements Server Functions
#'
#' @noRd
mod_nutrient_requirements_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    get_calf_req_ADG <- function(averBW           = 45,
                                 ADG              = 0.2,
                                 liq_diet         = 6,
                                 liq_diet_me      = 4.9,
                                 FPstarter        = 1,
                                 starter_composition = list(
                                   cs_ndf = 12,
                                   cs_nfc = 55,
                                   cs_cp = 22,
                                   cs_ee = 3,
                                   form_of_starter = "pelleted"),
                                 cs_intake_equation = "NASEM",
                                 liq_diet_dm      = 0.12,
                                 weaning_age       = 70,
                                 average_temperature         = 20,
                                 liq_diet_only    = FALSE,
                                 mature_weight    = 700,
                                 max_size         = 1) {

      weaned <- FALSE

      # retained energy

      EBG <- empty_weight_gain(ADG = ADG, liquid_diet_only = liqDietOnly, weaned = FALSE)

      EBW <- empty_body_weight(BW = averBW, liquid_diet_only = liqDietOnly, weaned = FALSE)

      liq_diet_all <- liq_diet # LD[1]

      liqDietIntake <- liq_diet_all * liqDietDM

      MEfromliqDiet <- liqDietIntake * liqDietME

      NASEM <- starter_intake_nasem(BW = averBW, MEiLD = MEfromliqDiet, FPstarter = FPstarter, temperature = average_temperature)

      starter_intake <- ifelse(liqDietOnly == TRUE & weaned == FALSE, 0, NASEM)

      nfc_intake_cum <- starter_intake * starter_composition$cs_nfc / 100



      #me_calf_starter <- me_calf_starter(ccsNFCI = nfc_intake_cum, pelleted = TRUE, texturized = TRUE)
      me_calf_starter <- starter_met_energy(cs_ndf = starter_composition$cs_ndf,
                                 cs_nfc = starter_composition$cs_nfc,
                                 cs_cp = starter_composition$cs_cp,
                                 cs_ee = starter_composition$cs_ee,
                                 nfc_intake = nfc_intake_cum)



      me_from_starter_intake <- starter_intake * me_calf_starter #solDietME

      total_dmi <- liqDietIntake + starter_intake

      total_me_intake <- MEfromliqDiet + me_from_starter_intake

      LiqDietRatio <- liqDietIntake / (liqDietIntake + starter_intake)

      me_temperature <- NEtemperature(EBW = averBW, age = 30, temperature = 20) # to do

      me_maintenance <- NEm(EBW, F) / km(liquid_diet_only = liqDietOnly) / 1000 + (me_temperature / km(liquid_diet_only = liqDietOnly))

      me_balance <- total_me_intake - me_maintenance

      me_ef_starter <- kg_solids(ME = 3.1)#me_calf_starter) #solDietME)

      MEefLiqDiet <- 0.55

      average_me_ef <- (me_ef_starter * (1 - LiqDietRatio) + (MEefLiqDiet * LiqDietRatio))

      NEg <- EBG ^ 1.1 * EBW ^ 0.205

      MEg <- NEg / average_me_ef

      totalME <- me_maintenance + MEg

      # Protein requirements

      scurf_cp <- scurf_cp(BW = averBW)

      urinary_cp <- endogenous_urinary_cp(BW = averBW)

      fecal_cp <- metabolic_fecal_cp(liq_diet_dmi = liqDietIntake, solid_diet_dmi = starter_intake)

      met_protein_maintenance <- MPmain(scurf_cp = scurf_cp, endogenous_urinary_cp = urinary_cp, metabolic_fecal_cp = fecal_cp)

      net_protein_gain <- net_protein_gain(EBW_gain = EBG, retained_energy = NEg)

      ef_met_protein_gain <- ef_met_protein_gain(BW = averBW, mature_weight = mature_weight)

      met_protein_gain <- net_protein_gain / ef_met_protein_gain

      total_met_protein_req <- met_protein_maintenance + met_protein_gain

      ef_met_protein_to_cp <- LiqDietRatio * 0.95 + (1 - LiqDietRatio) * 0.75

      total_cp_req <- total_met_protein_req / ef_met_protein_to_cp

      diet_cp_pct <- total_cp_req / 10 / total_dmi

      return(tibble::tibble(
        BW = averBW,
        ADG = ADG,
        EBWkg = EBW,
        EBG = EBG,
        Liq_diet_all = liq_diet_all,
        liqDietIntake = liqDietIntake,
        MEfromliqDiet = MEfromliqDiet,
        NASEMsi = NASEM,
        starter_intake = starter_intake,
        nfc_intake_cum = nfc_intake_cum,
        me_calf_starter = me_calf_starter,
        me_from_starter_intake = me_from_starter_intake,
        total_dmi = total_dmi,
        total_me_intake = total_me_intake,
        LiqDietRatio = LiqDietRatio,
        me_maintenance = me_maintenance,
        NEg = NEg,
        MEg = MEg,
        totalME = totalME,
        total_met_protein_req = total_met_protein_req,
        total_cp_req = total_cp_req
      ))

    }

    nasem <- reactive({

      tidyr::expand_grid(averBW = seq(input$min_peso, input$max_peso, input$int_peso),
                  ADG = seq(input$min_adg, input$max_adg, input$int_adg)) |>
        purrr::pmap(get_calf_req_ADG) |>
        purrr::map(dplyr::select, BW, ADG, me_maintenance, NEg, MEg, total_met_protein_req, total_cp_req) |>
        dplyr::bind_rows() |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3)))

    })

    output$NASEM <- DT::renderDataTable({
      nasem()
    }, options = list(paging = TRUE,
                      scrollY = "600px",
                      scrollX = "900px",
                      pageLength = 20),
    style = "bootstrap5")


  })
}

