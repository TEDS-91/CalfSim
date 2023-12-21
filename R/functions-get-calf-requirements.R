#' Calculates the calf requirements.
#'
#' @param liqDiet A numeric vector with the liquid diet intake.
#' @param liqDietME A numeric vector with the liquid diet metabolizable energy.
#' @param nfc_cs A numeric vector with the NFC content of the starter.
#' @param liqDietDM A numeric vector with the dry matter content of the liquid diet.
#' @param initBW A numeric vector with the initial body weight.
#' @param weaningAge A numeric vector with the weaning age.
#' @param averTemp A numeric vector with the average temperature.
#' @param liqDietOnly A logical vector indicating if the calf is fed only liquid diet.
#' @param mature_weight A numeric vector with the mature weight.
#' @param max_size A numeric vector with the maximum size of the vectors.
#'
#' @return A dataframe with the calf requirements.
#' @export
#'
#' @examples
#' get_calf_requirements(liqDiet = rep(6, 70), liqDietME = 4.6, nfc_cs = 66,
#' liqDietDM = 0.12, initBW = 45, weaningAge = 70, averTemp = 20,
#' liqDietOnly = FALSE, mature_weight = 750, max_size = 100)
get_calf_requirements <- function(liqDiet          = rep(6, 70),
                                  liqDietME        = 4.6,
                                  nfc_cs           = 66,
                                  #solDietME        = 3.2,
                                  liqDietDM        = 0.12,
                                  initBW           = 45,
                                  weaningAge       = 70,
                                  averTemp         = 20,
                                  liqDietOnly      = FALSE,
                                  mature_weight    = 750,
                                  max_size         = 100) {

  init_vectors <- c("daysOfLife",
                    "liquid_diet_only",
                    "weaned",
                    "temp",
                    "LiqDietAll",
                    "liqDietIntake",
                    "BW",
                    "BWcor",
                    "EBW",
                    "MEfromliqDiet",
                    "FPstarter",
                    "MEgap",
                    "starterIntake",
                    "MEgap",
                    "Quigley",
                    "NASEM",
                    "nfc_intake_cum",
                    "MEcs",
                    "MEfromSI",
                    "totalDMI",
                    "totalMEI",
                    "LiqDietRatio",
                    "MEtemp",
                    "MEmaint",
                    "MEbal",
                    "MEefS",
                    "MEefLiqDiet",
                    "averEf",
                    "netEnergyGain",
                    "EBWgain",
                    "ADG",
                    "fatGain",
                    "protGain",
                    "scurfCP",
                    "urinaryCP",
                    "fecalCP",
                    "metProtMain",
                    "metProtMain",
                    "npGain",
                    "efMPGain",
                    "metProtGain",
                    "totalMPReq",
                    "efMptoCp",
                    "reqCP",
                    "cpPctg"
  )

  for (i in init_vectors) {

    assign(i, vector(mode = "logical", length = max_size))

  }

  daysOfLife[1] <- 1

  liquid_diet_only[1] <- liqDietOnly[1]

  weaned[1] <- FALSE

  temp[1] <- averTemp

  BW[1] <- initBW

  EBW[1] <- empty_body_weight(BW[1], liquid_diet_only = liquid_diet_only[1], weaned = weaned[1])

  LiqDietAll[1] <- liqDiet[1] # LD[1]

  liqDietIntake[1] <- LiqDietAll[1] * liqDietDM

  MEfromliqDiet[1] <- liqDietIntake[1] * liqDietME

  FPstarter[1] <- 1 / 7

  #starterIntake[1] <- starter_intake_nasem(BW = BW[1], MEiLD = MEfromliqDiet[1], FPstarter = FPstarter[1], temperature = averTemp)

  #MEgap[1] <- MEgap(BW = BW[1], ADG = ADG[1], MEiLD = MEfromliqDiet[1])

  #Quigley[1] <- starter_intake_quigley(MEgap = MEgap[1], age = 1, temperature = averTemp, NDFDM = 18.3, PctForage = 0.6)

  NASEM[1] <- starter_intake_nasem(BW = BW[1], MEiLD = MEfromliqDiet[1], FPstarter = FPstarter[1], temperature = averTemp)

  starterIntake[1] <- ifelse(liquid_diet_only[1] == TRUE & weaned[1] == FALSE, 0, NASEM[1])

  nfc_intake_cum[1] <- starterIntake[1] * nfc_cs / 100

  MEcs[1] <- MEcs(ccsNFCI = nfc_intake_cum[1], pelleted = TRUE, texturized = TRUE) #TO DO

  MEfromSI[1] <- starterIntake[1] * MEcs[1] #solDietME

  totalDMI[1] <- liqDietIntake[1] + starterIntake[1]

  totalMEI[1] <- MEfromliqDiet[1] + MEfromSI[1]

  LiqDietRatio[1] <- liqDietIntake[1] / (liqDietIntake[1] + starterIntake[1])

  # NEtemp

  MEtemp[1] <- NEtemperature(EBW = EBW[1], age = daysOfLife[1], temperature = averTemp)

  MEmaint[1] <- NEm(EBW[1], F) / km(liquid_diet_only = liquid_diet_only[1]) / 1000 + (MEtemp[1] / km(liquid_diet_only = liquid_diet_only[1]))

  MEbal[1] <- totalMEI[1] - MEmaint[1]

  MEefS[1] <- kg_solids(ME = MEcs[1]) #solDietME)

  MEefLiqDiet[1] <- 0.55

  averEf[1] <- (MEefS[1] * (1 - LiqDietRatio[1]) + (MEefLiqDiet[1] * LiqDietRatio[1]))

  netEnergyGain[1] <- MEbal[1] * averEf[1]

  EBWgain[1] <- empty_body_weight_gain_from_re(retained_energy = netEnergyGain[1], EBW = EBW[1])

  ADG[1] <- ADG_from_empty_gain_weight(EGW = EBWgain[1], liquid_diet_only = liquid_diet_only[1], weaned = weaned[1])

  BWcor[1] <- BW[1] + ADG[1]

  fatGain[1] <- gain_composition(retained_energy = netEnergyGain[1])[[1]]

  protGain[1] <- gain_composition(retained_energy = netEnergyGain[1])[[2]]

  # Protein requirements

  scurfCP[1] <- scurf_cp(BW = BW[1])

  urinaryCP[1] <- endogenous_urinary_cp(BW = BW[1])

  fecalCP[1] <- metabolic_fecal_cp(liqDietDMI = liqDietIntake[1], solidDietDMI = starterIntake[1])

  metProtMain[1] <- MPmain(scurf_cp = scurfCP[1], endogenous_urinary_cp = urinaryCP[1], metabolic_fecal_cp = fecalCP[1])

  npGain[1] <- NPgain(EBWgain = EBWgain[1], retained_energy = netEnergyGain[1])

  efMPGain[1] <- efMPgain(BW = BW[1], mature_weight = mature_weight)

  metProtGain[1] <- npGain[1] / efMPGain[1]

  totalMPReq[1] <- metProtMain[1] + metProtGain[1]

  efMptoCp[1] <- LiqDietRatio[1] * 0.95 + (1 - LiqDietRatio[1]) * 0.75

  reqCP[1] <- totalMPReq[1] / efMptoCp[1]

  cpPctg[1] <- reqCP[1] / 10 / totalDMI[1]

  for (i in 2:max_size) {

    daysOfLife[i] <- i

    liquid_diet_only[i] <- liqDietOnly[1]

    weaned[i] <- ifelse(i <= weaningAge, FALSE, TRUE)

    temp[i] <- averTemp

    BW[i] <- BW[i-1] + ADG[i-1]

    EBW[i] <- empty_body_weight(BW[i], liquid_diet_only = liquid_diet_only[i], weaned = weaned[i])

    LiqDietAll[i] <- liqDiet[i] #LD[i]

    liqDietIntake[i] <- ifelse(i <= weaningAge, LiqDietAll[i] * liqDietDM, 0)

    MEfromliqDiet[i] <- liqDietIntake[i] * liqDietME

    FPstarter[i] <- i / 7

    #starterIntake[i] <- starter_intake_nasem(BW = BW[i], MEiLD = MEfromliqDiet[i], FPstarter = FPstarter[i], temperature = averTemp)

    #MEgap[i] <- MEgap(BW = BW[i], ADG = ADG[i], MEiLD = MEfromliqDiet[i])

    #Quigley[i] <- starter_intake_quigley(MEgap = MEgap[i], age = i, temperature = averTemp, NDFDM = 18.3, PctForage = 0.6)

    NASEM[i] <- starter_intake_nasem(BW = BW[i], MEiLD = MEfromliqDiet[i], FPstarter = FPstarter[i], temperature = averTemp)

    starterIntake[i] <- ifelse(liquid_diet_only[i] == TRUE & weaned[i] == FALSE, 0, NASEM[i])

    #MEfromSI[i] <- starterIntake[i] * solDietME

    nfc_intake_cum[i] <- sum(starterIntake[1:i]) * nfc_cs / 100

    MEcs[i] <- MEcs(ccsNFCI = nfc_intake_cum[i], pelleted = TRUE, texturized = TRUE)

    MEfromSI[i] <- starterIntake[i] * MEcs[i] #solDietME

    totalDMI[i] <- liqDietIntake[i] + starterIntake[i]

    totalMEI[i] <- MEfromliqDiet[i] + MEfromSI[i]

    LiqDietRatio[i] <- liqDietIntake[i] / (liqDietIntake[i] + starterIntake[i])

    # NEtemp

    MEtemp[i] <- NEtemperature(EBW = EBW[i], age = daysOfLife[i], temperature = averTemp)

    MEmaint[i] <- NEm(EBW[i], F) / km(liquid_diet_only = liquid_diet_only[i]) / 1000 + (MEtemp[i] / km(liquid_diet_only = liquid_diet_only[i]))

    MEbal[i] <- totalMEI[i] - MEmaint[i]

    MEefS[i] <- kg_solids(ME = MEcs[i]) #solDietME)

    MEefLiqDiet[i] <- 0.55

    averEf[i] <- (MEefS[i] * (1 - LiqDietRatio[i]) + (MEefLiqDiet[i] * LiqDietRatio[i]))

    netEnergyGain[i] <- MEbal[i] * averEf[i]

    EBWgain[i] <- empty_body_weight_gain_from_re(retained_energy = netEnergyGain[i], EBW = EBW[i])

    ADG[i] <- ADG_from_empty_gain_weight(EGW = EBWgain[i], liquid_diet_only = liquid_diet_only[i], weaned = weaned[i])

    BWcor[i] <- BW[i] + ADG[i]

    fatGain[i] <- gain_composition(retained_energy = netEnergyGain[i])[[1]]

    protGain[i] <- gain_composition(retained_energy = netEnergyGain[i])[[2]]

    # Protein requirements

    scurfCP[i] <- scurf_cp(BW = BW[i])

    urinaryCP[i] <- endogenous_urinary_cp(BW = BW[i])

    fecalCP[i] <- metabolic_fecal_cp(liqDietDMI = liqDietIntake[i], solidDietDMI = starterIntake[i])

    metProtMain[i] <- MPmain(scurf_cp = scurfCP[i], endogenous_urinary_cp = urinaryCP[i], metabolic_fecal_cp = fecalCP[i])

    npGain[i] <- NPgain(EBWgain = EBWgain[i], retained_energy = netEnergyGain[i])

    efMPGain[i] <- efMPgain(BW = BW[i], mature_weight = mature_weight)

    metProtGain[i] <- npGain[i] / efMPGain[i]

    totalMPReq[i] <- metProtMain[i] + metProtGain[i]

    efMptoCp[i] <- LiqDietRatio[i] * 0.95 + (1 - LiqDietRatio[i]) * 0.75

    reqCP[i] <- totalMPReq[i] / efMptoCp[i]

    cpPctg[i] <- reqCP[i] / 10 / totalDMI[i]

  }

  return(
    tibble::tibble(
      daysOfLife,
      liquid_diet_only,
      weaned,
      temp,
      BW,
      BWcor,
      EBW,
      LiqDietAll,
      liqDietIntake,
      MEfromliqDiet,
      FPstarter,
      #Quigley,
      NASEM,
      starterIntake,
      #MEgap,
      nfc_intake_cum,
      MEcs,
      MEfromSI,
      totalDMI,
      totalMEI,
      LiqDietRatio,
      MEtemp,
      MEmaint,
      MEbal,
      MEefS,
      MEefLiqDiet,
      averEf,
      netEnergyGain,
      EBWgain,
      ADG,
      fatGain,
      protGain,
      scurfCP,
      urinaryCP,
      fecalCP,
      metProtMain,
      npGain,
      efMPGain,
      metProtGain,
      totalMPReq,
      efMptoCp,
      reqCP,
      cpPctg
    )
  )

}
