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
                                 liqDiet          = 6,
                                 liqDietME        = 4.9,
                                 FPstarter        = 1,
                                 starter_composition = list(
                                   cs_ndf = 12,
                                   cs_nfc = 55,
                                   cs_cp = 22,
                                   cs_ee = 3
                                 ),
                                 liqDietDM        = 0.12,
                                 weaningAge       = 70,
                                 averTemp         = 20,
                                 liqDietOnly      = FALSE,
                                 mature_weight    = 700,
                                 max_size         = 1) {

      weaned <- FALSE

      # retained energy

      EBG <- empty_weight_gain(ADG = ADG, liquid_diet_only = liqDietOnly, weaned = FALSE)

      EBW <- empty_body_weight(BW = averBW, liquid_diet_only = liqDietOnly, weaned = FALSE)

      LiqDietAll <- liqDiet # LD[1]

      liqDietIntake <- LiqDietAll * liqDietDM

      MEfromliqDiet <- liqDietIntake * liqDietME

      NASEM <- starter_intake_nasem(BW = averBW, MEiLD = MEfromliqDiet, FPstarter = FPstarter, temperature = averTemp)

      starterIntake <- ifelse(liqDietOnly == TRUE & weaned == FALSE, 0, NASEM)

      nfc_intake_cum <- starterIntake * starter_composition$cs_nfc / 100



      #MEcs <- MEcs(ccsNFCI = nfc_intake_cum, pelleted = TRUE, texturized = TRUE)
      MEcs <- starter_met_energy(cs_ndf = starter_composition$cs_ndf,
                                 cs_nfc = starter_composition$cs_nfc,
                                 cs_cp = starter_composition$cs_cp,
                                 cs_ee = starter_composition$cs_ee,
                                 CSNFCI = nfc_intake_cum)



      MEfromSI <- starterIntake * MEcs #solDietME

      totalDMI <- liqDietIntake + starterIntake

      totalMEI <- MEfromliqDiet + MEfromSI

      LiqDietRatio <- liqDietIntake / (liqDietIntake + starterIntake)

      MEtemp <- NEtemperature(EBW = averBW, age = 30, temperature = 20) # to do

      MEmaint <- NEm(EBW, F) / km(liquid_diet_only = liqDietOnly) / 1000 + (MEtemp / km(liquid_diet_only = liqDietOnly))

      MEbal <- totalMEI - MEmaint

      MEefS <- kg_solids(ME = 3.1)#MEcs) #solDietME)

      MEefLiqDiet <- 0.55

      averEf <- (MEefS * (1 - LiqDietRatio) + (MEefLiqDiet * LiqDietRatio))

      NEg <- EBG ^ 1.1 * EBW ^ 0.205

      MEg <- NEg / averEf

      totalME <- MEmaint + MEg

      # Protein requirements

      scurfCP <- scurf_cp(BW = averBW)

      urinaryCP <- endogenous_urinary_cp(BW = averBW)

      fecalCP <- metabolic_fecal_cp(liqDietDMI = liqDietIntake, solidDietDMI = starterIntake)

      metProtMain <- MPmain(scurf_cp = scurfCP, endogenous_urinary_cp = urinaryCP, metabolic_fecal_cp = fecalCP)

      npGain <- NPgain(EBWgain = EBG, retained_energy = NEg)

      efMPGain <- efMPgain(BW = averBW, mature_weight = mature_weight)

      metProtGain <- npGain / efMPGain

      totalMPReq <- metProtMain + metProtGain

      efMptoCp <- LiqDietRatio * 0.95 + (1 - LiqDietRatio) * 0.75

      reqCP <- totalMPReq / efMptoCp

      cpPctg <- reqCP / 10 / totalDMI

      return(tibble::tibble(
        BW = averBW,
        ADG = ADG,
        EBWkg = EBW,
        EBG = EBG,
        LiqDietAll = LiqDietAll,
        liqDietIntake = liqDietIntake,
        MEfromliqDiet = MEfromliqDiet,
        NASEMsi = NASEM,
        starterIntake = starterIntake,
        nfc_intake_cum = nfc_intake_cum,
        MEcs = MEcs,
        MEfromSI = MEfromSI,
        totalDMI = totalDMI,
        totalMEI = totalMEI,
        LiqDietRatio = LiqDietRatio,
        MEmaint = MEmaint,
        NEg = NEg,
        MEg = MEg,
        totalME = totalME,
        totalMPReq = totalMPReq,
        reqCP = reqCP
      ))

    }

    nasem <- reactive({

      tidyr::expand_grid(averBW = seq(input$min_peso, input$max_peso, input$int_peso),
                  ADG = seq(input$min_adg, input$max_adg, input$int_adg)) |>
        purrr::pmap(get_calf_req_ADG) |>
        purrr::map(dplyr::select, BW, ADG, MEmaint, NEg, MEg, totalMPReq, reqCP) |>
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

## To be copied in the UI
# mod_nutrient_requirements_ui("nutrient_requirements_1")

## To be copied in the server
# mod_nutrient_requirements_server("nutrient_requirements_1")
